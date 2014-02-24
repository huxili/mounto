%%
%% Trace utility
%%    [1] Production system tracing (YES).
%%    [2] Develepment phase tracing (YES).
%%
%% References:
%%
%%


-module(mto_trace).
-vsn("1.0.0").

-export([trace/3, trace/4]).

%% ------------
%% API
%% ------------

-spec trace(M::atom(), F::atom(), Args::any()) -> {ignore, trace_inactive}|{ignore, msg_empty}|{ok, logged}.

trace(M, F, Args) when is_atom(M) ->
    A = application:get_application(),
    E = application:get_env(list_to_atom(atom_to_list(M) ++ "_trace")),  % ex: {mto_upnp_trace, true}
    case {A, E} of
       {undefined, _} -> trace_impl(M, F, Args);  % turn on : isolated module
       {_App, undefined} -> {ignore, trace_inactive};      % turn off : no flag
       {_App, {ok,false}} -> {ignore, trace_inactive};     % turn off : flag/false
       {_App, {ok,_}} -> trace_impl(M, F, Args)   % turn on otherwise
   end.


trace(M, F, MsgFormat, Msg) when is_atom(M) ->
    try io_lib:format(MsgFormat, Msg) of
        Msg1 -> trace(M, F, Msg1)
    catch _:_
        -> trace(M, F, Msg)
    end.




%% ---------------------------------------
%% Internal
%% ---------------------------------------
trace_impl(_Domain, _F, []) -> {ignore, msg_empty};
trace_impl(Domain, F, Args) ->
    {{Yr, Mn, Day},{Hr, Min, Sec}} = erlang:localtime(),
    try io_lib:format("~p-~p-~p ~p:~p:~p [~p:~p] ~ts\r\n", [Yr, Mn, Day, Hr, Min, Sec, Domain, F, Args]) of
        Msg -> error_logger:info_msg(Msg)
    catch _:_
        -> error_logger:info_msg("~p-~p-~p ~p:~p:~p [~p:~p] ~p\r\n", [Yr, Mn, Day, Hr, Min, Sec, Domain, F, Args])
    end,
    {ok, logged}.

%%-----------------------------------------------------------------------
%% Eunit testing code.
%%
%% Test from erlang shell :
%%      c(mto_trace, [debug_info, {d, 'EUNIT'}]), eunit:test(mto_trace).
%%
%%-----------------------------------------------------------------------
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-define(PERF_REPEAT, 1000).

trace_test() ->
     trace(?MODULE,trace_test, ["annd", " A simple Msg"]),
     trace(?MODULE,trace_test, "~s, ~s", ["annd", " A simple Msg"]),
     trace(?MODULE,trace_test, "~s~f", ["annd", " A simple Msg"]),
     trace(?MODULE,trace_test, "~s~f", [{"annd", " A simple Msg"}]),
     trace(?MODULE,trace_test, [{"annd", " A simple Msg"}]),
     trace(?MODULE,trace_test, "A simple Msg"),
     ?assert(trace(?MODULE,trace_test, "") == {ignore, msg_empty}),
     ?assert(trace(?MODULE,trace_test, hello) == {ok, logged}).

perf_test() ->
     F = fun(N) ->
         Fun = fun(F1,N1) ->
            case N1 of
               0 -> ok;
               _ -> trace(?MODULE,perf_test, ""), F1(F1, N1-1)
            end
         end,
         Fun(Fun, N)
     end,
     ?debugVal(timer:tc(F, [?PERF_REPEAT])).

-endif.

