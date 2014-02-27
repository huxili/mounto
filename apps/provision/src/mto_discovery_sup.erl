-module(mto_discovery_sup).
-vsn("1.0.0").

-behaviour(application).
-behaviour(supervisor).
-define(APPLICATION, mto_discovery).
-define(SUPERVISOR, mto_discovery_sup).

%% API
-export([start/0, stop/0]).

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

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    Name = {local, ?SUPERVISOR},
    supervisor:start_link(Name, ?MODULE, []).

stop(_State) ->
    ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    Discovery = {mto_discovery, {mto_discovery, start_event, []}, permanent, 5000, worker, [mto_discovery]},
    Bonjour = {mto_bonjour, {mto_bonjour, start_link, []}, permanent, 5000, worker, [mto_bonjour]},
    Upnp = {mto_upnp, {mto_upnp, start_link, []}, permanent, 5000, worker, [mto_upnp]},
    Netbios = {mto_netbios, {mto_netbios, start_link, []}, permanent, 5000, worker, [mto_netbios]},
    {ok, {
           {one_for_one, 5, 10},
           [Discovery, Bonjour, Upnp, Netbios]
         }
    }.


%%-----------------------------------------------------------------------
%% Eunit testing code.
%%
%% Test from erlang shell :
%%      c(mto_discovery_sup, [debug_info, {d, 'EUNIT'}]), eunit:test(mto_discovery_sup).
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