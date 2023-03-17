-module(server3).

-export([start/2, rpc/2, swap_code/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Server with hot code swapping  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Name, Mod) ->
    register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

rpc(Name, Request) ->
    Name ! {self(), Request},
    receive
        {_, crash} ->
            exit(rpc);
        {_, ok, Response} ->
            Response
    end.

swap_code(Name, Mod) ->
    rpc(Name, {swap_code, Mod}).

loop(Name, Mod, OldState) ->
    receive
        {From, {swap_code, NewCallBackMod}} ->
            From ! {Name, ack},
            loop(Name, NewCallBackMod, OldState);
        {From, Request} ->
            try Mod:handle(Request, OldState) of
                {Response, NewState} ->
                    From ! {Name, Response},
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
