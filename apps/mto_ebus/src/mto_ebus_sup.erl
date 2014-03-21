-module(mto_ebus_sup).
-vsn("1.0.0").

-behaviour(application).
-behaviour(supervisor).
-define(APPLICATION, mto_ebus).
-define(SUPERVISOR, mto_ebus_sup).

%% API
-export([start/0, stop/0, restart/0]).

%% Application callbacks
-export([start/2, stop/1]).
%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
start() ->
    application:start(?APPLICATION).

stop() ->
    application:stop(?APPLICATION).

restart() ->
    stop(), start().

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    Name = {local, ?SUPERVISOR},
    Traced = mto_trace:traced(?MODULE),
    mto_trace:trace(Traced, ?APPLICATION, start, application:get_all_env()),
    supervisor:start_link(Name, ?MODULE, []).

stop(_State) ->
    ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    process_flag(trap_exit, true),
    Ebus = {mto_ebus, {mto_ebus, start, [event_manager]}, permanent, 5000, worker, [mto_ebus]},
    {ok, {
           {one_for_one, 5, 10},
           [Ebus]
         }
    }.


%%-----------------------------------------------------------------------
%% Eunit testing code.
%%
%% Test from erlang shell :
%%      c(mto_ebus_sup, [debug_info, {d, 'EUNIT'}]), eunit:test(mto_ebus_sup).
%%
%%-----------------------------------------------------------------------
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
    ?debugVal(start()),
    ?debugVal(application:which_applications()),
    Pid = whereis(?SUPERVISOR), ?assert(is_pid(Pid)).



stop_test() ->
    ?debugVal(stop()),
    undefined = whereis(?SUPERVISOR).


-endif.
