-module(functions).
-compile(export_all).

% With the help of the cons operator(|)
head([H|_]) -> H.

second([_, X|_]) -> X.

same(X, X) ->
    true;
same(_, _) ->
    false.

valid_time({Date={Y, M, D}, Time={H, Min, S}}) ->
    io:format("The Data tuple (~p) says today is:~p/~p/~p,~n", [Date, Y, M, D]),
    io:format("The time tuple(~p) indicates: ~p:~p.~p.~n", [Time, H, Min, S]);
valid_time(_) ->
    io:format("Stop feeding me wrong data!~n").
