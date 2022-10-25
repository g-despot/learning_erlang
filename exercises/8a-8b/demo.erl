-module(demo).
-export([start_proc/0, start_two_proc/1]).

%% 8A Ping
start_proc() ->
    spawn(fun wait_for_message/0).

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

%% 8B Ping-pong
start_two_proc(M) ->
    Pid1 = start_proc(),
    Pid2 = start_proc(),
    ping_pong(Pid1, Pid2, M).

ping_pong(Pid1, Pid2, M) when M > 0 ->
    Pid1 ! {self(), ping},
    receive
        X ->
            Pid2 ! X,
            ping_pong(Pid2, Pid1, M - 1)
    end;
ping_pong(Pid1, Pid2, 0) ->
    Pid1 ! stop,
    Pid2 ! stop.
