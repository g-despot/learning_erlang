-module(ms).
-compile([export_all]).

worker(WorkerId) ->
    receive
        {From, Ref, {"Who there?"}} ->
            From ! {Ref, WorkerId};
        {From, Ref, {"U alive bro?"}} ->
            From ! {Ref, "Yup"};
        {From, Ref, {"Bye"}} ->
            From ! {Ref, "Bye"};
        {_, _, {"die"}} ->
            exit(WorkerId)
    end,
    worker(WorkerId).

start(N) ->
    Pid = spawn(?MODULE, boss, [N]),
    register(boss, Pid).

boss(N) ->
    process_flag(trap_exit, true),
    WorkersList = [{I, spawn_link(?MODULE, worker, [I])} || I <- lists:seq(1, N)],
    loop(maps:from_list(WorkersList)).

loop(WorkersMap) ->
    receive
        {Ref, N, {Message}} ->
            io:format("BOSS: ~p~n", [Message]),
            maps:get(N, WorkersMap) ! {self(), Ref, {Message}};
        {_, Response} ->
            io:format("WORKER: ~p~n", [Response]);
        {'EXIT', Pid, normal} ->
            io:format("EXIT: ~p~n", [Pid]),
            ok;
        % manual shutdown, not a crash
        {'EXIT', Pid, shutdown} ->
            io:format("EXIT: ~p~n", [Pid]),
            ok;
        {'EXIT', Pid, WorkerId} ->
            io:format("RESTART: ~p ID: ~p~n", [Pid, WorkerId]),
            UpdatedWorkersMap = WorkersMap#{WorkerId := spawn_link(?MODULE, worker, [WorkerId])},
            loop(UpdatedWorkersMap)
    end,
    loop(WorkersMap).

to_slave(Message, N) ->
    Ref = make_ref(),
    boss ! {Ref, N, {Message}}.
