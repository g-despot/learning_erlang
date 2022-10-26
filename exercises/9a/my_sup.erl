-module(my_sup).
-compile([export_all]).

myproc() ->
    timer:sleep(5000),
    exit(reason).

chain(0) ->
    receive
        _ -> ok
    after 2000 ->
        exit("chain dies here")
    end;
chain(N) ->
    Pid = spawn(fun() -> chain(N - 1) end),
    link(Pid),
    receive
        _ -> ok
    end.

server() ->
    receive
        {From, Ref, {"Hello"}} ->
            From ! {Ref, "Hello to you too!"};
        {From, Ref, {"How are you?"}} ->
            From ! {Ref, "Great!"};
        {From, Ref, {"Bye"}} ->
            From ! {Ref, "Bye"};
        {From, Ref, {_Other}} ->
            From ! {Ref, "WTF"}
    end,
    server().

start_server_sup() ->
    spawn(?MODULE, restarter, []).

start_server() ->
    Pid = spawn(?MODULE, server, []),
    register(server, Pid).

restarter() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, server, []),
    register(server, Pid),
    receive
        % not a crash
        {'EXIT', Pid, normal} ->
            ok;
        % manual shutdown, not a crash
        {'EXIT', Pid, shutdown} ->
            ok;
        {'EXIT', Pid, _} ->
            restarter()
    end.

client(Request) ->
    Ref = make_ref(),
    server ! {self(), Ref, {Request}},
    receive
        {Ref, Response} -> Response
    after 2000 ->
        timeout
    end.
