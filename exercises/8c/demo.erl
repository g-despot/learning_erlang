-module(demo).
-export([start_proc/0]).
-import(cl_sim, [start/1]).

-define(pin, [1, 2, 3, 4]).
-define(pin_size, 4).

%% 8A Ping
start_proc() ->
    %% This is the process that waits for user input and sends the
    %% PIN to the process executig code_lock, in this case self().
    LogicPid = spawn(fun wait_for_message/0),

    %% This is the interface process for entering the PIN.
    SimPid = cl_sim:start(LogicPid),

    %% This is the process that closes and opens the lock.
    code_lock(LogicPid, SimPid).

code_lock(LogicPid, SimPid) ->
    LogicPid ! {self(), ready},
    receive
        Pin when is_list(Pin) ->
            case pin_correct(Pin) of
                true ->
                    io:format("The PIN ~p is correct.~n", [Pin]),
                    SimPid ! {display, "Open"},
                    timer:sleep(5000),
                    SimPid ! {display, "Locked"};
                false ->
                    io:format("The PIN ~p is incorrect.~n", [Pin]),
                    SimPid ! {display, "Locked"}
            end;
        Other ->
            io:format("Got unexpected: ~p~n", [Other])
    end,
    code_lock(LogicPid, SimPid).

wait_for_message() ->
    receive
        {Pid, ready} ->
            io:format("code_lock is ready with Pid: ~p~n", [Pid]),
            wait_for_message(Pid, ?pin_size, []);
        Other ->
            io:format("Got unexpected message from code_lock: ~p~n", [Other])
    end.
wait_for_message(Pid, N, Pin) when N > 0 ->
    receive
        {button, Button} ->
            io:format("Pin input: ~p~n", [Button]),
            wait_for_message(Pid, N - 1, Pin ++ [Button]);
        Other ->
            io:format("Got unexpected message: ~p~n", [Other]),
            wait_for_message(Pid, N, Pin)
    end;
wait_for_message(Pid, 0, Pin) ->
    Pid ! Pin,
    wait_for_message().

pin_correct(Pin) ->
    if
        Pin =:= ?pin ->
            true;
        true ->
            false
    end.
