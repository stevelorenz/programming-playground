-module(lib_misc).

-export([for/3, qsort/1, pythag/1, perms/1, odds_and_evens1/1, odds_and_evens2/1, sqrt/1,
         on_exit/2, keep_alive/2, string2value/1]).

% Base case
for(Max, Max, F) ->
    [F(Max)];
for(I, Max, F) ->
    [F(I) | for(I + 1, Max, F)].

qsort([]) ->
    [];
qsort([Pivot | T]) ->
    qsort([X || X <- T, X < Pivot]) ++ [Pivot] ++ qsort([X || X <- T, X >= Pivot]).

pythag(N) ->
    [{A, B, C}
     || A <- lists:seq(1, N),
        B <- lists:seq(1, N),
        C <- lists:seq(1, N),
        A + B + C =< N,
        A * A + B * B =:= C * C].

perms([]) ->
    [[]];
perms(L) ->
    %% -- is the list substraction operation
    [[H | T] || H <- L, T <- perms(L -- [H])].

odds_and_evens1(L) ->
    Odds = [X || X <- L, X rem 2 =:= 1],
    Evens = [],
    {Odds, Evens}.

odds_and_evens2(L) ->
    odds_and_evens_acc(L, [], []).

odds_and_evens_acc([H | T], Odds, Evens) ->
    case H rem 2 of
        1 ->
            odds_and_evens_acc(T, [H | Odds], Evens);
        0 ->
            odds_and_evens_acc(T, Odds, [H | Evens])
    end;
odds_and_evens_acc([], Odds, Evens) ->
    {Odds, Evens}.

sqrt(X) when X < 0 ->
    error({squareRootNegativeArgument, X});
sqrt(X) ->
    math:sqrt(X).

on_exit(Pid, Fun) ->
    spawn(fun() ->
             % Create a process monitor to receive DOWN message when the Pid process dies.
             Ref = erlang:monitor(process, Pid),
             receive {'DOWN', Ref, process, Pid, Why} -> Fun(Why) end
          end).

keep_alive(Name, Fun) ->
    register(Name, Pid = spawn(Fun)), % Use name instead of Pid
    on_exit(Pid, fun(_Why) -> keep_alive(Name, Fun) end).

string2value(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    Bindings = erl_eval:new_bindings(),
    {value, Value, _} = erl_eval:expr(Exprs, Bindings),
    Value.
