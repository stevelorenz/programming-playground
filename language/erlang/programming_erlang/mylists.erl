-module(mylists).

-export([sum/1, map/2]).

sum([]) ->
    0;
sum([H | T]) ->
    H + sum(T).

map(_, []) ->
    [];
map(F, [H | T]) ->
    [F(H) | map(F, T)].
