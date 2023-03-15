-module(a).

-export([start/1, loop/1]).

start(Tag) ->
    spawn(fun() -> loop(Tag) end).

loop(Tag) ->
    sleep(),
    Val = b:x(),
    io:format("Vsn1 (~p) b:x() = ~p~n", [Tag, Val]),
    loop(Tag).

sleep() ->
    receive after 3000 ->
        true
    end.
