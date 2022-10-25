-module(demo).
-export([start_proc/2]).

%% 8D The star
wait_for_message() ->
    receive
        {FromPid, ping} ->
            io:format("~p got ~p~n", [FromPid, ping]),
            FromPid ! {self(), pong};
        stop ->
            exit(stop);
        Other ->
            io:format("Got ~p~n", [Other])
    end,
    wait_for_message().

start_proc(N, M) ->
    Processes = lists:map(fun(_) -> spawn(fun wait_for_message/0) end, lists:seq(1, N)),
    spawn(fun() -> play_ping_pong(Processes, Processes, M) end).

play_ping_pong([ToPid | Tail], AllProcesses, M) when M > 0, length(Tail) > 0 ->
    ToPid ! {self(), ping},
    receive
        Message ->
            io:format("Node returned: ~p~n", [Message]),
            play_ping_pong(Tail, AllProcesses, M)
    end;
play_ping_pong([ToPid | Tail], AllProcesses, M) when M > 0, length(Tail) =:= 0 ->
    ToPid ! {self(), ping},
    receive
        Message ->
            io:format("Node returned: ~p~n", [Message]),
            play_ping_pong(AllProcesses, AllProcesses, M - 1)
    end;
play_ping_pong(_, AllProcesses, 0) ->
    [Proc ! stop || Proc <- AllProcesses],
    exit(stop).
