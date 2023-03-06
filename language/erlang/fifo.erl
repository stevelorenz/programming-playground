-module(fifo).
-export([new/0, push/2, pop/1, empty/1]).

%% Implement an FIFO queue with two stacks and pattern matching

new() -> {fifo, [], []}.

push({fifo, In, Out}, X) -> {fifo, [X|In], Out}.

pop({fifo, [], []}) -> erlang:error("empty FIFO");
% Reverse the first stack and put all items in the second stack
pop({fifo, In, []}) -> pop({fifo, [], lists:reverse(In)});
pop({fifo, In, [H|T]}) -> {H, {fifo, In, T}}.

empty({fifo, [], []}) -> true;
empty({fifo, _, _}) -> false.
