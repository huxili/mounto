
-module(mto_ebus).
-vsn("1.1.0").
-behaviour(gen_event).
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("include/mto_ebus.hrl").

%% API
-export([start/0, stop/0, restart/0]). % Start/stop application.
-export([start/1, stop/1]). % Start/stop event server, called by supervisor.
-export([handlers/0]). % Event API.

%% gen_event
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-define(EVENT_SERVER, mto_ebus).
-define(EVENT_DB, mto_ebus_db).

% Internal: State
-record(state, {event_db, trace=false, args}).

% Internal:  protocol
-record(mto, {cat=ebus, cmd, args=[]}).


%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
start() ->
    mto_ebus_sup:start(). % start application

stop() ->
    mto_ebus_sup:stop().  % stop application

restart() ->
    stop(), start().

start(event_manager) -> % Internal API called by supervisor
    R = gen_event:start_link({local, ?EVENT_SERVER}),
    gen_event:add_handler(?EVENT_SERVER, ?MODULE, []),
    R.

stop(event_manager) ->
    gen_event:stop(?EVENT_SERVER).

handlers() ->
    gen_event:which_handlers(?EVENT_SERVER).


%% ------------------------------------------------------------------
%% gen_event (call back)
%% ------------------------------------------------------------------
init(Args) ->
  process_flag(trap_exit, true),
  Db = init_db(),
  Traced = mto_trace:traced(?MODULE),
  {ok, #state{event_db=Db,trace=Traced, args = Args}}.

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
   Tab = ets:new(?EVENT_DB, [bag]),
   Tab.

handle_any_event(Msg, State) ->
    mto_trace:trace(?MODULE, handle_any_event, Msg),
    {ok, State}.


%%-----------------------------------------------------------------------
%% Eunit testing code.
%%
%% Test from erlang shell :
%%      c(mto_ebus, [debug_info, {d, 'EUNIT'}]), eunit:test(mto_ebus).
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

unknown_msg_test() ->
    ?debugVal(start(event_manager)),
    ?EVENT_SERVER ! impossible_msg,
    stop(event_manager).

-endif.
