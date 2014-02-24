%%
%% UPNP discovery of devices
%%
%% References:
%%   [Wiki, FR] http://fr.wikipedia.org/wiki/Protocole_UPnP
%%   [Wiki, EN] http://en.wikipedia.org/wiki/Internet_Gateway_Device_Protocol
%%   [IPv6] http://ipv6friday.org/blog/2011/12/ipv6-multicast/
%%   [LG] http://developer.lgappstv.com/TV_HELP/index.jsp?topic=%2Flge.tvsdk.references.book%2Fhtml%2FUDAP%2FUDAP%2FM+SEARCH+Request.htm
%%
%%

-module(mto_upnp).
-vsn("1.0.4").
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").

% API
-export([start/0, start_link/0, stop/0, ping/0]).
-export([probe/0, probe/1]).
-export([dump/0, dump/1]).
-export([responses/0, responses/1]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define(SERVER, ?MODULE).
-define(RESPONSE_DB, mto_upnp_db).
-define(RESPONSE_DB_MODE, protected).
-define(LOCAL_PORT, 1900).
-define(UPNP_PORT, 1900).
-define(UPNP_ADDR, {239,255,255,250}).
-define(UPNP_ADDRv6, {16#FF02,0,0,0,0,0,0,16#F}).
-define(SOCKET_OPTS, [{reuseaddr,true}, {broadcast, true}, {active, true}]).
-define(UPNP_SEARCH_ALL, "M-SEARCH * HTTP/1.1\r\n" "Host: 239.255.255.250:1900\r\n" "Man: \"ssdp:discover\"\r\n" "ST: ssdp:all\r\n" "MX: 3\r\n\r\n").
-define(upnp_search(ST), "M-SEARCH * HTTP/1.1\r\n" "Host: 239.255.255.250:1900\r\n" "Man: \"ssdp:discover\"\r\n" ++ ST ++ "MX: 3\r\n\r\n").
-define(DEFAULT_RESPONSE_FILENAME, 'upnp_responses.ets').

% Internal: Server State
-record(state, {socket, ping=0, db}).

% Internal:  protocol
-record(mto, {cat=upnp, cmd, args=[]}).

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
        undefined -> ok;
        _ -> gen_server:call(?SERVER, #mto{cmd=stop})
    end.

ping() ->
    case whereis(?SERVER) of
        undefined -> {error, pang};
        _ -> gen_server:call(?SERVER, #mto{cmd=ping})
    end.

probe() ->
    probe_impl(?UPNP_SEARCH_ALL).
probe(ST) ->
    case ST of
       [$S|[$T|[$:|_]]] ->  probe_impl(?upnp_search(ST));
       _ -> erlang:error(badarg, {upnp0001,"ST invalid when calling probe"})
    end.

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

%% Cmd: Dump_response
handle_call(#mto{cmd=dump_response, args=[Filename]}, _From, State) ->
    R = dump_response_db(Filename),
    {reply, R, State};

handle_call(Request, _From, State) ->
    error_logger:info_msg("any_cmd ~p \r\n", [Request]),
    {reply, ok, State}.

%% Cmd: Search
handle_cast(#mto{cmd=search, args=[SearchMsg]}, State) ->
    handle_cast_search(SearchMsg,State),
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
    handle_cast_search(?UPNP_SEARCH_ALL,State),
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
probe_impl(SearchMsg) ->
   case whereis(?SERVER) of
      undefined -> {error, server_inactive};
      _ -> gen_server:cast(?SERVER, #mto{cmd=search, args=[SearchMsg]})
   end.

dump_reponse_impl(Filename) ->
   case whereis(?SERVER) of
      undefined -> {error, server_inactive};
      _ -> gen_server:call(?SERVER, #mto{cmd=dump_response, args=[Filename]})
   end.


%% --------------------------------------
%% Callback Internal
%% -------------------------------------
init_db() ->
   ets:new(?RESPONSE_DB, [bag, {keypos,#response.addr}, named_table, ?RESPONSE_DB_MODE]).

handle_cast_search(SearchMsg,State) ->
   mto_trace:trace(?SERVER, search, "cast ~p", [SearchMsg]),
   search_impl(SearchMsg, State),
   State.

handle_info_response({udp, FromIp, FromPort, Msg}, State) ->
   Host = inet:gethostbyaddr(FromIp),
   ets:insert(?RESPONSE_DB, #response{addr=FromIp, hostent=Host, port=FromPort, response=Msg}),
   State.

search_impl(SearchMsg, State) ->
   gen_udp:send(State#state.socket, ?UPNP_ADDR, ?UPNP_PORT, SearchMsg),
   gen_udp:send(State#state.socket, ?UPNP_ADDRv6, ?UPNP_PORT, SearchMsg),
   State.

dump_response_db(Filename)->
   R = ets:tab2file(?RESPONSE_DB, Filename),
   mto_trace:trace(?SERVER, dump_response, R),
   R.



%%-----------------------------------------------------------------------
%% Eunit testing code.
%%
%% Test from erlang shell :
%%      c(mto_upnp, [debug_info, {d, 'EUNIT'}]), eunit:test(mto_upnp).
%%
%%-----------------------------------------------------------------------
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
    RStart = start_link(), ?debugVal(RStart),
    ?debugVal(ets:info(?RESPONSE_DB)),
    case RStart of
      {ok, Pid} -> {ok, Pid} = RStart;  % Patten match
      {error, _} -> {error, {already_started, _}} = RStart
    end.

ping_test() ->
   start_link(), RPing = ping(), ?debugVal(RPing),
   ?assert({ok,pong} == RPing),
   stop(), RPing1 = ping(), ?debugVal(RPing1),
   ?assert({error,pang} == RPing1).

stop_test() ->
    start_link(), RStop = stop(), ?debugVal(RStop),
    RStopPing = ping(), ?assert({error,pang} == RStopPing).

probe_test()->
    start_link(), Rprobe=probe(), ?debugVal(Rprobe),
    Rprobe1=probe("ST: upnp:rootdevice\r\n"),?debugVal(Rprobe1),
    ?debugVal(probe("ST:urn:Microsoft Windows Peer Name Resolution Protocol: V4:IPV6:LinkLocal\r\n")),
    ?assertError(badarg, probe("0ST: upnp:rootdevice\r\n")).

dump_test() ->
    start_link(), ?assert({error,eaccess} == dump('<')),
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

