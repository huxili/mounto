%%
%% Bonjour discovery of devices
%%
%% References:
%%   [Wiki] http://en.wikipedia.org/wiki/Bonjour_(software)
%%   [IPv6] http://ipv6friday.org/blog/2011/12/ipv6-multicast/
%%
%%

-module(mto_bonjour).
-vsn("1.1.0").
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("kernel/src/inet_dns.hrl").

% API
-export([start/0, start_link/0, stop/0, ping/0]).
-export([sub/0, sub/1]).
-export([dump/0, dump/1]).
-export([responses/0, responses/1]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define(SERVER, ?MODULE).
-define(DB_SUFFIX, _db).
-define(RESPONSE_DB, mto_bonjour_db).
-define(RESPONSE_DB_MODE, protected).
-define(LOCAL_PORT, 5353).
-define(MDNS_PORT, 5353).
-define(MDNS_ADDR, {224,0,0,251}).
-define(MDNS_ADDRv6, {16#FF02,0,0,0,0,0,0,16#FB}).
-define(SOCKET_OPTS, [{reuseaddr,true}, {broadcast, true}, {active, true}, binary]).
-define(DOMAIN_LOCAL, "_services._dns-sd._udp.local").
-define(mdns_search(Domain), (Domain)).
-define(DEFAULT_RESPONSE_FILENAME, 'mdns_responses.ets').

% Internal: Server State
-record(state, {
              socket,
              subscriptions=[],
              services=[],
              answers=[],
              ping=0,
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

sub() ->
    sub_impl(?DOMAIN_LOCAL).
sub(Domain) ->
    sub_impl(Domain).

dump() ->
    dump_reponse_impl(?DEFAULT_RESPONSE_FILENAME).
dump(Filename) ->
    dump_reponse_impl(Filename).

%% Raw responses data, opaque.
responses() ->
    ets:tab2list(?RESPONSE_DB).

%%
% Ex: responses(hostent) =>
%      [{ok,{hostent,"PCLI",[],inet,4,[{192,168,5,10}]}},
%      {ok,{hostent,"MACLI",[],inet,4,[{192,168,5,16}]}}]
%%
responses(hostent) ->
    MS = ets:fun2ms(fun(#response{hostent=H} = R) -> H end),
    L = ets:select(?RESPONSE_DB, MS),
    sets:to_list(sets:from_list(L));

%%
% Ex:  responses(host) => ["PCLI","MACLI"]
%%
responses(host) ->
    MS = ets:fun2ms(fun(#response{hostent={_,{_1,H,_3,_4,_5,_6}}} = R) -> H end),
    L = ets:select(?RESPONSE_DB, MS),
    sets:to_list(sets:from_list(L));

%% Ex: responses(addr) => [{192,168,5,75},{192,168,5,24}]
responses(addr) ->
    MS = ets:fun2ms(fun(#response{addr=A} = R) -> A end),
    L = ets:select(?RESPONSE_DB, MS),
    sets:to_list(sets:from_list(L));

%% reponses({10,100,5,75}) =>
responses(Addr) ->
    MS = ets:fun2ms(fun(#response{addr=A} = R) when A == Addr -> R end),
    ets:select(?RESPONSE_DB, MS).


%% ------------------------------------------------------------------
%% gen_server (call back)
%% ------------------------------------------------------------------

init([]) ->
  process_flag(trap_exit, true),
  {ok, S} = gen_udp:open(?LOCAL_PORT, ?SOCKET_OPTS),
  inet:setopts(S, [{add_membership, {{224,0,0,251}, {0,0,0,0}}}]),
  Db = init_db(),
  mto_trace:trace(?SERVER, init, "ok"),
  {ok, #state{socket = S, ping=0, db=Db}, 5000}.

%% Cmd: Stop
handle_call(#mto{cmd=stop}, _From, State) ->
     mto_trace:trace(?SERVER, stop, "ok"),
    {stop, normal, {ok,stop}, State};

%% Cmd: Ping
handle_call(#mto{cmd=ping}, From, State) ->
    NPing = State#state.ping +1,
    mto_trace:trace(?SERVER, ping, "from ~p [~p]", [From, NPing]),
    NewState = State#state{ping=NPing},
    {reply, {ok, pong}, NewState};

handle_call(Request, _From, State) ->
    error_logger:info_msg("any_cmd ~p \r\n", [Request]),
    {reply, ok, State}.

%% Cmd: Search
handle_cast(#mto{cmd=search, args=[Domain]}, State) ->
    handle_cast_search(Domain,State),
    {noreply, State};

%% Cmd: Dump_response
handle_cast(#mto{cmd=dump_response, args=[Filename]}, State) ->
    dump_response_db(Filename),
    {noreply, State};

handle_cast(_Msg, State) ->
    mto_trace:trace(?SERVER, cast, _Msg),
    {noreply, State}.

handle_info({udp, _S, FromIp, FromPort, Msg}, State) ->
    handle_info_response({udp, FromIp, FromPort, Msg}, State),
    {noreply, State};
handle_info(timeout, State) ->
    % Start search automatically after start (timeout)
    handle_cast_search(?DOMAIN_LOCAL,State),
    {noreply, State};
handle_info(_Info, State) ->
    mto_trace:trace(?SERVER, info, _Info),
    {noreply, State}.

terminate(Reason, State) ->
    inet:close(State#state.socket),
    mto_trace:trace(?SERVER, terminate, Reason),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ------------------------------------------------------------------
%% API Internal
%% ------------------------------------------------------------------
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

%% --------------------------------------
%% Callback Internal
%% -------------------------------------
init_db() ->
   Tab = ets:new(?RESPONSE_DB, [bag, {keypos,#response.addr}, named_table, ?RESPONSE_DB_MODE]),
   mto_trace:trace(?SERVER, init_db, ok),
   Tab.

handle_cast_search(Domain,State) ->
   mto_trace:trace(?SERVER, search, "cast ~p", [Domain]),
   search_impl(Domain, State),
   State.

handle_info_response({udp, FromIp, FromPort, Msg}, State) ->
   mto_trace:trace(?SERVER, response, FromIp),
   Host = inet:gethostbyaddr(FromIp),
   Msg1 = inet_dns:decode(Msg),
   ets:insert(?RESPONSE_DB, #response{addr=FromIp, hostent=Host, port=FromPort, response=Msg1}),
   State.

search_impl(Domain, State) ->
   Queries = [#dns_query{type=ptr, domain=Domain, class=in},
              #dns_query{type=srv, domain=Domain, class=in}],
   Out = #dns_rec{header=#dns_header{}, qdlist=Queries},
   gen_udp:send(State#state.socket, ?MDNS_ADDR, ?MDNS_PORT, inet_dns:encode(Out)),
   gen_udp:send(State#state.socket, ?MDNS_ADDRv6, ?MDNS_PORT, inet_dns:encode(Out)),
   State.

dump_response_db(Filename)->
   R = ets:tab2file(?RESPONSE_DB, Filename),
   mto_trace:trace(?SERVER, dump_response, R),
   R.



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
    ?assert({error, server_inactive}==dump()).

start_test() ->
    ?debugVal(start()),
    ?debugVal(stop()),
    RStart = start_link(), ?debugVal(RStart),
    {ok, _Pid} = RStart,  % Patten match
    {error, {already_started, _}} = start_link().

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
    Rsub1=sub("_services._dns-sd._tcp.local"),?debugVal(Rsub1).

dump_test() ->
    start_link(), ?debugVal(dump()),
    ?debugVal(dump('<')),
    ?assert(ok == dump('test.ets')).

response_test() ->
    start_link(), ?debugVal(responses()).
response_hostent_test() ->
    start_link(), ?debugVal(responses(hostent)).
response_host_test() ->
    start_link(), ?debugVal(responses(host)).
response_addr_test() ->
    start_link(), ?debugVal(responses({10,100,5,18})),
    ?debugVal(responses(addr)).

-endif.

