-module(motor_controller).

-export([add_event_handler/0]).

add_event_handler() ->
    add_event_handler:add_handler(errors, fun controller/1).

controller(too_hot) ->
    io:format("Turn off the monitor");
controller(X) ->
    io:format("~w ignored event:~p~n", [?MODULE, X]).
