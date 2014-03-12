%%
%% Netnios- discovery of devices (OS Windows only)
%%
%% References:
%%   [Wiki] http://en.wikipedia.org/wiki/NetBIOS
%%   [Port] 138
%%
%%

-module(mto_netbios).
-vsn("1.0.1").
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").

% API
-export([start/0, start_link/0, stop/0, ping/0]).
-export([dump/0, dump/1]).
-export([responses/0, responses/1]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define(SERVER, ?MODULE).
-define(DB_SUFFIX, _db).
-define(RESPONSE_DB, mto_netbios_db).
-define(LOCAL_PORT, 138).
-define(DEFAULT_RESPONSE_FILENAME, 'netbios_responses.ets').

% Internal: Server State
-record(state, {socket, ping=0, db}).

% Internal:  protocol
-record(mto, {cat=netbios, cmd, args=[]}).

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

dump() ->
    dump_reponse_impl(?DEFAULT_RESPONSE_FILENAME).
dump(Filename) ->
    dump_reponse_impl(Filename).

%% Raw responses data, opaque.
responses() ->
    case whereis(?SERVER) of
        undefined -> [];
        _ -> ets:tab2list(?RESPONSE_DB)
    end.



%%
% Ex: responses(hostent) =>
%      [{ok,{hostent,"PCLI",[],inet,4,[{192,168,5,10}]}},
%      {ok,{hostent,"MACLI",[],inet,4,[{192,168,5,16}]}}]
%%
responses(hostent) ->
    case whereis(?SERVER) of
        undefined -> [];
        _ -> MS = ets:fun2ms(fun(#response{hostent=H} = R) -> H end),
             L = ets:select(?RESPONSE_DB, MS),
             sets:to_list(sets:from_list(L))
    end;

%%
% Ex:  responses(host) => ["PCLI","MACLI"]
%%
responses(host) ->
    case whereis(?SERVER) of
        undefined -> [];
        _ -> MS = ets:fun2ms(fun(#response{hostent={_,{_1,H,_3,_4,_5,_6}}} = R) -> H end),
             L = ets:select(?RESPONSE_DB, MS),
             sets:to_list(sets:from_list(L))
    end;

%% Ex: responses(addr) => [{192,168,5,75},{192,168,5,24}]
responses(addr) ->
    case whereis(?SERVER) of
        undefined -> [];
        _ -> MS = ets:fun2ms(fun(#response{addr=A} = R) -> A end),
             L = ets:select(?RESPONSE_DB, MS),
             sets:to_list(sets:from_list(L))
    end;


%% reponses({10,100,5,75}) =>
responses(Addr) ->
    case whereis(?SERVER) of
        undefined -> [];
        _ -> MS = ets:fun2ms(fun(#response{addr=A} = R) when A == Addr -> R end),
             ets:select(?RESPONSE_DB, MS)
    end.


%% ------------------------------------------------------------------
%% gen_server (call back)
%% ------------------------------------------------------------------

init([]) ->
  process_flag(trap_exit, true),
  {Osfamily, _Osname} = os:type(),
  case Osfamily of
      win32 ->
           Opts = [{reuseaddr,true}, {broadcast, true}, {active, true}, binary],
           {ok, S} = gen_udp:open(?LOCAL_PORT, Opts),
           Db = init_db(),
           mto_trace:trace(?SERVER, init, "ok"),
           {ok, #state{socket = S, ping=0, db=Db}};
      Others ->
           mto_trace:trace(?SERVER, init, "~p ~p", [Others, unsupported]),
           ignore
  end.


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
dump_reponse_impl(Filename) ->
   case whereis(?SERVER) of
      undefined -> {error, server_inactive};
      _ -> gen_server:call(?SERVER, #mto{cmd=dump_response, args=[Filename]})
   end.

%% --------------------------------------
%% Callback Internal
%% -------------------------------------
init_db() ->
   Tab = ets:new(?RESPONSE_DB, [set, {keypos,#response.addr}, named_table]),
   mto_trace:trace(?SERVER, init_db, ok),
   Tab.

handle_info_response({udp, FromIp, FromPort, Msg}, State) ->
   Host = inet:gethostbyaddr(FromIp),
   Msg1 = inet_dns:decode(Msg),
   mto_discovery:register_pnode({netbios, FromIp, FromPort, Msg1}),
   ets:insert(?RESPONSE_DB, #response{addr=FromIp, hostent=Host, port=FromPort, response=Msg1}),
   State.

dump_response_db(Filename)->
   R = ets:tab2file(?RESPONSE_DB, Filename),
   mto_trace:trace(?SERVER, dump_response, R),
   R.


%%-----------------------------------------------------------------------
%% Eunit testing code.
%%
%% Test from erlang shell :
%%      c(mto_netbios, [debug_info, {d, 'EUNIT'}]), eunit:test(mto_netbios).
%%
%%-----------------------------------------------------------------------
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
    RStart = start_link(), ?debugVal(RStart),
    ?debugVal(ets:info(?RESPONSE_DB)),
    case RStart of
      {ok, Pid} -> {ok, Pid} = RStart;  % Patten match
      ignore -> ?debugVal({ignore, "server inactive"});
      {error, _} -> {error, {already_started, _}} = RStart
    end,
    stop().

ping_test() ->
   start_link(), RPing = ping(), ?debugVal(RPing),
   case whereis(?SERVER) of
      undefined -> {error, server_inactive};
      _ -> ?assert({ok,pong} == RPing)
   end,
   stop(), RPing1 = ping(), ?debugVal(RPing1),
   ?assert({error,pang} == RPing1).

stop_test() ->
    start_link(), RStop = stop(), ?debugVal(RStop),
    RStopPing = ping(), ?assert({error,pang} == RStopPing).

dump_test() ->
    start_link(),
    ?debugVal(dump('test.ets')),
    file:delete('test.ets'), stop().

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

