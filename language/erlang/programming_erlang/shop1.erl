-module(shop1).

-export([total/1]).

%% Nice !
total([{What, N} | T]) ->
    shop:cost(What) * N + total(T);
total([]) ->
    0.
