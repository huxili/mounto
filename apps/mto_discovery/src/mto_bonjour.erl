%%
%% Bonjour discovery of devices
%%
%% References:
%%   [Wiki] http://en.wikipedia.org/wiki/Bonjour_(software)
%%   [IPv6] http://ipv6friday.org/blog/2011/12/ipv6-multicast/
%%
%%

-module(mto_bonjour).
-vsn("1.2.1").
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("kernel/src/inet_dns.hrl").

% API
-export([start/0, start_link/0, stop/0, ping/0, restart/0]).
-export([sub/0, sub/1, advertise/0]).
-export([dump/0, dump/1]).
-export([responses/0, responses/1]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define(SERVER, ?MODULE).
-define(RESPONSE_DB, mto_bonjour_db).
-define(LOCAL_PORT, 5353).
-define(MDNS_PORT, 5353).
-define(MDNS_ADDR, {224,0,0,251}).
-define(MDNS_ADDRv6, {16#FF02,0,0,0,0,0,0,16#FB}).
-define(SSD_DOMAIN_PREFIX, "_services._dns-sd._udp.").
-define(DOMAIN_LOCAL, "local").
-define(DEFAULT_RESPONSE_FILENAME, 'mdns_responses.ets').

% Internal: Server State
-record(state, {
              socket,
              ttl = 4500, % in ms
              mto_port=8888,
              subscriptions=[],
              services=[],
              answers=[],
              ping=0,
              trace=false,
              db}).

% Internal:  protocol
-record(mto, {cat=bonjour, cmd, args=[]}).

% Internal: db
-record (response, {addr, hostent, port, response}).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

restart() ->
    stop(), start().


stop()->
    case whereis(?SERVER) of
        undefined -> {ok, stop};
        _ -> gen_server:call(?SERVER, #mto{cmd=stop})
    end.

ping() ->
    case whereis(?SERVER) of
        undefined -> {error, pang};
        _ -> gen_server:call(?SERVER, #mto{cmd=ping})
    end.


advertise() ->
    advertise_impl(?DOMAIN_LOCAL).
advertise(Domain) ->
    advertise_impl(Domain).

sub() ->
    sub_impl(?DOMAIN_LOCAL).
sub(Domain) ->
    sub_impl(Domain).

dump() ->
    dump_reponse_impl(?DEFAULT_RESPONSE_FILENAME).
dump(Filename) ->
    dump_reponse_impl(Filename).

responses() ->
    ets:tab2list(?RESPONSE_DB).

responses(hostent) ->
    MS = ets:fun2ms(fun(#response{hostent=H} = R) -> H end),
    L = ets:select(?RESPONSE_DB, MS),
    sets:to_list(sets:from_list(L));

responses(host) ->
    MS = ets:fun2ms(fun(#response{hostent={_,{_1,H,_3,_4,_5,_6}}} = R) -> H end),
    L = ets:select(?RESPONSE_DB, MS),
    sets:to_list(sets:from_list(L));

responses(addr) ->
    MS = ets:fun2ms(fun(#response{addr=A} = R) -> A end),
    L = ets:select(?RESPONSE_DB, MS),
    sets:to_list(sets:from_list(L));

responses(Addr) ->
    MS = ets:fun2ms(fun(#response{addr=A} = R) when A == Addr -> R end),
    ets:select(?RESPONSE_DB, MS).

%% ------------------------------------------------------------------
%% API Impl (L1)
%% ------------------------------------------------------------------
advertise_impl(Domain) ->
   case whereis(?SERVER) of
      undefined -> {error, server_inactive};
      _ -> gen_server:cast(?SERVER, #mto{cmd=advertise, args=[Domain]})
   end.

sub_impl(Domain) ->
   case whereis(?SERVER) of
      undefined -> {error, server_inactive};
      _ -> gen_server:cast(?SERVER, #mto{cmd=search, args=[Domain]})
   end.

dump_reponse_impl(Filename) ->
   case whereis(?SERVER) of
      undefined -> {error, server_inactive};
      _ -> gen_server:cast(?SERVER, #mto{cmd=dump_response, args=[Filename]})
   end.


%% ------------------------------------------------------------------
%% gen_server (call back)
%% ------------------------------------------------------------------
init([]) ->
  process_flag(trap_exit, true),
  Opts = [{reuseaddr,true}, {broadcast, true}, {active, true},
          {multicast_loop, false}, { multicast_ttl, 255},
          {add_membership, {?MDNS_ADDR, {0,0,0,0}}},
          binary
          ],
  S = case gen_udp:open(?LOCAL_PORT, Opts) of
         {ok, Socket} -> Socket;
         _ -> {ok, Socket} = gen_udp:open(0, Opts), Socket
      end,
  Db = init_db(),
  Traced = mto_trace:traced(?MODULE),
  erlang:send_after(500, self(), #mto{cmd=search, args=[?DOMAIN_LOCAL]}),
  erlang:send_after(1000, self(), #mto{cmd=advertise, args=[?DOMAIN_LOCAL]}),
  mto_trace:trace(Traced, ?SERVER, init, "ok"),
  {ok, #state{socket = S, ping=0, trace=Traced, db=Db}}.

%% Cmd: Stop
handle_call(#mto{cmd=stop}, _From, State) ->
    mto_trace:trace(State#state.trace, ?SERVER, stop, "ok"),
    {stop, normal, {ok,stop}, State};

%% Cmd: Ping
handle_call(#mto{cmd=ping}, From, State) ->
    NPing = State#state.ping +1,
    mto_trace:trace(State#state.trace, ?SERVER, ping, "from ~p [~p]", [From, NPing]),
    NewState = State#state{ping=NPing},
    {reply, {ok, pong}, NewState};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% Cmd: Search
handle_cast(#mto{cmd=search, args=[Domain]}, State) ->
    mto_trace:trace(State#state.trace, ?SERVER, search, Domain),
    handle_cast_search(Domain,State),
    erlang:send_after(crypto:rand_uniform(60000, 600000), self(),
                      #mto{cmd=search, args=[Domain]}),
    {noreply, State};

%% Cmd: Advertise
handle_cast(#mto{cmd=advertise, args=[Domain]}, State) ->
    mto_trace:trace(State#state.trace, ?SERVER, advertise, Domain),
    handle_cast_advertise(Domain,State),
    erlang:send_after(crypto:rand_uniform(60000, 600000), self(),
                     #mto{cmd=advertise, args=[Domain]}),
    {noreply, State};

%% Cmd: Dump_response
handle_cast(#mto{cmd=dump_response, args=[Filename]}, State) ->
    mto_trace:trace(State#state.trace, ?SERVER, dump_response, Filename),
    ets:tab2file(?RESPONSE_DB, Filename),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(#mto{cmd=search, args=[_Domain]}=Cmd, State) ->
    handle_cast(Cmd, State),
    {noreply, State};

handle_info(#mto{cmd=advertise, args=[_Domain]}=Cmd, State) ->
    handle_cast(Cmd, State),
    {noreply, State};
% Inbound Msg
handle_info({udp, _S, FromIp, FromPort, Msg}, State) ->
    handle_info_response({udp, FromIp, FromPort, Msg}, State),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    mto_trace:trace(State#state.trace, ?SERVER, terminate, Reason),
    inet:close(State#state.socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% --------------------------------------
%% Callback Impl (L1)
%% -------------------------------------
init_db() ->
   Tab = ets:new(?RESPONSE_DB, [bag, {keypos,#response.addr}, named_table]),
   Tab.

handle_cast_search(Domain,State) ->
   Message = message(search, Domain, State),
   gen_udp:send(State#state.socket, ?MDNS_ADDR, ?MDNS_PORT, inet_dns:encode(Message)),
   gen_udp:send(State#state.socket, ?MDNS_ADDRv6, ?MDNS_PORT, inet_dns:encode(Message)),
   State.

handle_cast_advertise(Domain,State) ->
   M = message(advertise, node, Domain, State),
   gen_udp:send(State#state.socket, ?MDNS_ADDR, ?MDNS_PORT, inet_dns:encode(M)),
   gen_udp:send(State#state.socket, ?MDNS_ADDRv6, ?MDNS_PORT, inet_dns:encode(M)),
   M1 = message(advertise, service, Domain, State),
   gen_udp:send(State#state.socket, ?MDNS_ADDR, ?MDNS_PORT, inet_dns:encode(M1)),
   gen_udp:send(State#state.socket, ?MDNS_ADDRv6, ?MDNS_PORT, inet_dns:encode(M1)),
   State.

handle_info_response({udp, FromIp, FromPort, Msg}, State) ->
   Host = inet:gethostbyaddr(FromIp),
   Msg1 = inet_dns:decode(Msg),
   mto_discovery:register_pnode({bonjour, FromIp, FromPort, Msg1}),
   ets:insert(?RESPONSE_DB, #response{addr=FromIp, hostent=Host, port=FromPort, response=Msg1}),
   do_reply_query({udp, FromIp, FromPort, Msg1}, State),
   State.


%% --------------------------------
%% Callback Impl (L2)
%% --------------------------------
message(search, Domain, _State) ->
   SSD = ?SSD_DOMAIN_PREFIX ++ Domain,
   Queries = [#dns_query{type=ptr, domain=SSD, class=in},
              #dns_query{type=srv, domain=SSD, class=in}],
   Msg = #dns_rec{header=#dns_header{}, qdlist=Queries}, Msg.

message(advertise,node, Domain, State) ->
   inet_dns:make_msg([{header, header(advertise)},
                      {anlist, answers(advertise,node,Domain, State)},
                      {arlist, resources(advertise,Domain, State)}]);
message(advertise,service, Domain, State) ->
   inet_dns:make_msg([{header, header(advertise)},
                      {anlist, answers(advertise,service,Domain, State)},
                      {arlist, resources(advertise,Domain, State)}]).



header(advertise) ->
   inet_dns:make_header([{id,0},
                         {qr,true},
                         {opcode,query},
                         {aa,true},
                         {tc,false},
                         {rd,false},
                         {ra,false},
                         {pr,false},
                         {rcode,0}]).

answers(advertise, node, Domain, #state{ttl = TTL, mto_port=Port} = State) ->
   SSD = ?SSD_DOMAIN_PREFIX ++ Domain,
   MTO = "_mtonode._tcp." ++ Domain,
   Node = atom_to_list(node()),
   LNode = Node ++ "." ++ MTO,
   [
    inet_dns:make_rr([{type, ptr}, {domain, SSD}, {class, in}, {ttl, TTL}, {data, MTO}]),
    inet_dns:make_rr([{type, ptr}, {domain, MTO}, {class, in}, {ttl, TTL}, {data, LNode}])
   ];

answers(advertise, service, Domain, #state{ttl = TTL, mto_port=Port} = State) ->
   MTO = "_mtonode._tcp." ++ Domain,
   Node = atom_to_list(node()),
   LNode = Node ++ "." ++ MTO,
   [
    inet_dns:make_rr([{type, ptr}, {domain, MTO}, {class, in}, {ttl, TTL}, {data, LNode}]),
    inet_dns:make_rr([{type, srv}, {domain, LNode}, {class, in}, {ttl, TTL}, {data, {0, 0, Port, Node}}])
   ].

resources(advertise, Domain, #state{ttl = TTL, mto_port=Port} = State) ->
   [].

 do_reply_query({udp, FromIp, FromPort, Msg}, State) ->
   S = inet:sockname(State#state.socket),
   mto_trace:trace(State#state.trace, ?SERVER, reply, S),
   mto_trace:trace(State#state.trace, ?SERVER, reply, "~p ~p ~n", [FromIp, FromPort]),
   mto_trace:trace(State#state.trace, ?SERVER, reply, Msg),
   ok.

%%-----------------------------------------------------------------------
%% Eunit testing code.
%%
%% Test from erlang shell :
%%      c(mto_bonjour, [debug_info, {d, 'EUNIT'}]), eunit:test(mto_bonjour).
%%
%%-----------------------------------------------------------------------
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


exception_test() ->
    ?assert({error, server_inactive}==sub()),
    ?assert({error, server_inactive}==advertise()),
    ?assert({error, server_inactive}==dump()).

start_test() ->
    ?debugVal(start()),
    {timeout, 15, ?debugVal(stop())},
    RStart = start_link(), ?debugVal(RStart),
    {ok, _Pid} = RStart,  % Patten match
    {error, {already_started, _}} = start_link(),
    stop().

restart_test() ->
    ?debugVal(start()),
    ?debugVal(restart()),
    stop().

ping_test() ->
   start_link(), RPing = ping(), ?debugVal(RPing),
   ?assert({ok,pong} == RPing),
   stop(), RPing1 = ping(), ?debugVal(RPing1),
   ?assert({error,pang} == RPing1).

stop_test() ->
    ?assert({ok, stop} == stop()),
    start_link(), RStop = stop(), ?debugVal(RStop),
    RStopPing = ping(), ?assert({error,pang} == RStopPing).

sub_test()->
    start_link(), Rsub=sub(), ?debugVal(Rsub),
    Rsub1=sub("_services._dns-sd._tcp.local"),?debugVal(Rsub1),
    stop().

advertise_test()->
    start_link(), R=advertise(), ?debugVal(R),
    R1=advertise("_services._dns-sd._tcp.local"),?debugVal(R1),
    stop().

dump_test() ->
    start_link(),
    ?debugVal(dump()),file:delete(?DEFAULT_RESPONSE_FILENAME),
    ?assert(ok == dump('test.ets')), file:delete('test.ets'),
    stop().

response_test() ->
    start_link(), ?debugVal(responses()), stop().
response_hostent_test() ->
    start_link(), ?debugVal(responses(hostent)), stop().
response_host_test() ->
    start_link(), ?debugVal(responses(host)), stop().
response_addr_test() ->
    start_link(), ?debugVal(responses({10,100,5,18})),
    ?debugVal(responses(addr)), stop().

-endif.

