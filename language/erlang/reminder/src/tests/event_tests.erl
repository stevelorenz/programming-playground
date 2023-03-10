-module(event_tests).
-include_lib("eunit/include/eunit.hrl").
-test_warnings([start/0, start_link/1, init/0, time_to_go/1]).

-record(state, {server,
                name="",
                to_go=[0]}).

timeout_test_() ->
    S = self(),
    spawn_link(event, loop, [#state{server=S, name="test", to_go=[2]}]),
    timer:sleep(1000),
    M1 = receive A -> A after 0 -> timeout end,
    timer:sleep(1500),
    M2 = receive B -> B after 0 -> timeout end,
    M3 = receive C -> C after 0 -> timeout end,
    [?_assertEqual(timeout, M1),
     ?_assertEqual({done, "test"}, M2),
     ?_assertEqual(timeout, M3)].
