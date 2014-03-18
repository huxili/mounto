%%
%% Trace utility
%%    [1] Production system tracing (YES).
%%    [2] Develepment phase tracing (YES).
%%
%% References:
%%
%%


-module(mto_trace).
-vsn("1.1.0").

-export([traced/1, trace/4, trace/5]).

%% ------------
%% API
%% ------------
traced(M) ->
   A = application:get_application(),
   E = application:get_env({M, trace}), % ex: {{mto_upnp, trace}, true}},
   case {A, E} of
       {undefined, _} -> true;       % turn on : isolated module
       {_App, undefined} -> false;   % turn off : no flag
       {_App, {ok,true}} -> true;    % turn on
       {_App, {ok,_}} -> false       % turn off
   end.

trace(true, M, F, Args) when is_atom(M) ->
    trace_impl(M, F, Args);
trace(false, M, _F, _Args) when is_atom(M) ->
    {ignore, trace_inactive};
trace(_Active, M, _F, _Args) when is_atom(M) ->
    {ignore, trace_inactive}.

trace(true, M, F, MsgFormat, Msg) when is_atom(M) ->
    try io_lib:format(MsgFormat, Msg) of
         Msg1 -> trace(true, M, F, Msg1)
    catch _:_
         -> trace(true, M, F, Msg)
    end;
trace(false, M, _F, _MsgFormat, _Msg) when is_atom(M) ->
    {ignore, trace_inactive};
trace(_Active, M, _F, _MsgFormat, _Msg) when is_atom(M) ->
    {ignore, trace_inactive}.


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
-define(PERF_REPEAT, 10000).

traced_test() ->
    ?assert(traced(mto_trace)).

trace_test() ->
     trace(true, ?MODULE,trace_test, ["annd", " A simple Msg"]),
     trace(true, ?MODULE,trace_test, "~s, ~s", ["annd", " A simple Msg"]),
     trace(true, ?MODULE,trace_test, "~s~f", ["annd", " A simple Msg"]),
     trace(true, ?MODULE,trace_test, "~s~f", [{"annd", " A simple Msg"}]),
     trace(true, ?MODULE,trace_test, [{"annd", " A simple Msg"}]),
     trace(true, ?MODULE,trace_test, "A simple Msg"),
     ?assert(trace(true, ?MODULE,trace_test, "") == {ignore, msg_empty}),
     ?assert(trace(true, ?MODULE,trace_test, hello) == {ok, logged}).

perf_test() ->
     F = fun(N) ->
         Fun = fun(F1,N1) ->
            case N1 of
               0 -> ok;
               _ -> trace(true, ?MODULE,perf_test, ""), F1(F1, N1-1)
            end
         end,
         Fun(Fun, N)
     end,
     ?debugVal(timer:tc(F, [?PERF_REPEAT])).

-endif.

