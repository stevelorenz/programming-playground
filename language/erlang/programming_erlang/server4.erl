-module(server4).

-export([loop/3, start/2, swap_code/2]).

start(Name, Mod) ->
    register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

swap_code(Name, Mod) ->
    rpc(Name, {swap_code, Mod}).

rpc(Name, Request) ->
    Name ! {self(), Request},
    receive
        {_, crash} ->
            exit(rpc);
        {_, ok, Response} ->
            Response
    end.

loop(Name, Mod, OldState) ->
    receive
        {From, {swap_code, NewCallBackMod}} ->
            From ! {Name, ok, ack},
            loop(Name, NewCallBackMod, OldState);
        {From, Request} ->
            try Mod:handle(Request, OldState) of
                {_, NewState} ->
                    From ! {Name, ok, Request},
                    loop(Name, Mod, NewState)
            catch
                _:Why ->
                    log_or_error(Name, Request, Why),
                    From ! {Name, crash},
                    loop(Name, Mod, OldState)
            end
    end.

log_or_error(Name, Request, Why) ->
    io:format("Server ~p request ~p ~ncaused exception ~p~n", [Name, Request, Why]).
