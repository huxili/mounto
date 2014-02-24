-module(mto_discovery).
-vsn("1.0.0").

-behaviour(application).
-behaviour(supervisor).

%% API
-export([start/0, stop/0]).

%% Application callbacks
-export([start/2, stop/1]).
%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    {ok, { {one_for_one, 5, 10},
     [?CHILD(mto_bonjour,worker),
      ?CHILD(mto_upnp,worker),
      ?CHILD(mto_netbios,worker)]} }.

