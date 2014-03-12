
-module(mto_discovery).
-vsn("1.0.0").
-behaviour(gen_event).
-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([start/0, stop/0]). % Start/stop application.
-export([start/1, stop/1]). % Start/stop event server, called by supervisor.
-export([handlers/0]).      % All registered event handlers.
-export([register_pnode/1, pnode/1]).

%% gen_event
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-define(EVENT_SERVER, mto_discovery).
-define(DISCOVERED_PNODE_DB, mto_pnode_db).

% Internal: State
-record(state, {pnode_db, args}).

% Internal:  protocol
-record(mto, {cat=discovery, cmd, args=[]}).

% Internal: db
-record (pnode, {addr, host, port, ts, source=[]}).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
start() ->
    mto_discovery_sup:start(). % start application

stop() ->
    mto_discovery_sup:stop().  % stop application

start(event_manager) -> % Internal API called by supervisor
    R = gen_event:start_link({local, ?EVENT_SERVER}),
    gen_event:add_handler(?EVENT_SERVER, ?MODULE, []),
    R.

stop(event_manager) ->
    gen_event:stop(?EVENT_SERVER).

handlers() ->
    gen_event:which_handlers(?EVENT_SERVER).

register_pnode({upnp, FromIP, FromPort, RawMsg}) ->
   case whereis(?EVENT_SERVER) of
       undefined -> {error, {?EVENT_SERVER, down}};
       _ -> gen_event:notify(?EVENT_SERVER, {upnp, FromIP, FromPort, RawMsg})
   end;
register_pnode({bonjour, FromIP, FromPort, RawMsg}) ->
   case whereis(?EVENT_SERVER) of
       undefined -> {error, {?EVENT_SERVER, down}};
       _ -> gen_event:notify(?EVENT_SERVER, {bonjour, FromIP, FromPort, RawMsg})
   end;
register_pnode({netbios, FromIP, FromPort, RawMsg}) ->
   case whereis(?EVENT_SERVER) of
       undefined -> {error, {?EVENT_SERVER, down}};
       _ -> gen_event:notify(?EVENT_SERVER, {netbios, FromIP, FromPort, RawMsg})
   end.

pnode(addr) ->
    case whereis(?EVENT_SERVER) of
       undefined -> {error, {?EVENT_SERVER, down}};
       _ -> MS = ets:fun2ms(fun(#pnode{addr=A} = R) -> A end),
            ets:select(?DISCOVERED_PNODE_DB, MS)
   end.

%% ------------------------------------------------------------------
%% gen_event (call back)
%% ------------------------------------------------------------------
init(Args) ->
  process_flag(trap_exit, true),
  Db = init_db(),
  {ok, #state{pnode_db=Db, args = Args}}.

handle_event(Event, State) ->
    handle_any_event(Event, State).

handle_info(Msg, State) ->
    handle_any_event(Msg, State),
    {ok, State}.

handle_call(Msg, State) ->
    Reply = ok,
    {ok, NewState} = handle_any_event(Msg, State),
    {ok, Reply, NewState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% API Internal
%% ------------------------------------------------------------------

%% --------------------------------------
%% Callback Internal
%% -------------------------------------
init_db() ->
   Tab = ets:new(?DISCOVERED_PNODE_DB, [set, {keypos,#pnode.addr}, named_table]),
   mto_trace:trace(?MODULE, init_db, ok),
   Tab.

handle_any_event({upnp, FromIp, FromPort, RawMsg}, State) ->
   add_pnode_impl({upnp, FromIp, FromPort, RawMsg}, State),
   {ok, State};

handle_any_event({bonjour, FromIp, FromPort, RawMsg}, State) ->
   add_pnode_impl({bonjour, FromIp, FromPort, RawMsg}, State),
   {ok, State};

handle_any_event({netbios, FromIp, FromPort, RawMsg}, State) ->
   add_pnode_impl({netbios, FromIp, FromPort, RawMsg}, State),
   {ok, State};

handle_any_event(Msg, State) ->
    mto_trace:trace(?MODULE, handle_any_event, Msg),
    {ok, State}.

add_pnode_impl({Source, FromIp, FromPort, RawMsg}, State) ->
   Host = case inet:gethostbyaddr(FromIp) of
               {ok, {_,H,_,_,_,_}} -> H;
               _ -> undefined
          end,
   F = fun(E) ->  case E of Source -> true; _ -> false end end,
   case ets:lookup(?DISCOVERED_PNODE_DB, FromIp) of
     [] -> ets:insert(?DISCOVERED_PNODE_DB,
           #pnode{addr=FromIp, host=Host, port=FromPort, ts=now(), source=[Source]});
     [PNode|_] -> #pnode{source = S} = PNode,
           case lists:any(F, S) of
                false -> ets:insert(?DISCOVERED_PNODE_DB, PNode#pnode{source=[Source|S], ts=now()});
                true -> ets:insert(?DISCOVERED_PNODE_DB, PNode#pnode{ts=now()})
           end
   end,
   ok.


%%-----------------------------------------------------------------------
%% Eunit testing code.
%%
%% Test from erlang shell :
%%      c(mto_discovery, [debug_info, {d, 'EUNIT'}]), eunit:test(mto_discovery).
%%
%%-----------------------------------------------------------------------
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    ?debugVal(start()),
    Pid = whereis(?EVENT_SERVER), ?assert(is_pid(Pid)),
    ?debugVal(stop()), undefined = whereis(?EVENT_SERVER).

start_stop_event_test() ->
    ?debugVal(start(event_manager)),
    Pid = whereis(?EVENT_SERVER), ?assert(is_pid(Pid)),
    ?debugVal(stop(event_manager)), undefined = whereis(?EVENT_SERVER).

handlers_test() ->
    ?debugVal(start(event_manager)),
    ?debugVal(handlers()),
    Pid = whereis(?EVENT_SERVER), ?assert(is_pid(Pid)),
    stop(event_manager).

register_pnode_upnp_test() ->
    ?debugVal(start(event_manager)),
    ?debugVal(register_pnode({upnp, {127,0,0,1}, 1900, no_msg})),
    stop(event_manager),
    {error,_} = register_pnode({upnp, {127,0,0,1}, 1900, no_msg}).

register_pnode_bonjour_test() ->
    ?debugVal(start(event_manager)),
    ?debugVal(register_pnode({bonjour, {127,0,0,1}, 1900, no_msg})),
    stop(event_manager),
    {error,_} = register_pnode({upnp, {127,0,0,1}, 1900, no_msg}).

register_pnode_netbios_test() ->
    ?debugVal(start(event_manager)),
    ?debugVal(register_pnode({netbios, {127,0,0,1}, 1900, no_msg})),
    stop(event_manager),
    {error,_} = register_pnode({upnp, {127,0,0,1}, 1900, no_msg}).

pnode_test() ->
    ?debugVal(start(event_manager)),
    ?debugVal(register_pnode({netbios, {127,0,0,1}, 1900, no_msg})),
    ?debugVal(pnode(addr)),
    stop(event_manager),
    ?debugVal(pnode(addr)).

-endif.
