%%%
%%% Template for Exercise 9: Quit-Error-Spawn
%%%

-module(qes).

-export([start/0, start/1, init/0, init/1]).
-include_lib("wx/include/wx.hrl").

start() ->
    spawn(?MODULE, init, []).

start(ParentPid) ->
    spawn_link(?MODULE, init, [ParentPid]).

init() ->
    make_window(),
    process_flag(trap_exit, true),
    loop(self()).

init(ParentPid) ->
    make_window(),
    loop(ParentPid).

loop(ParentPid) ->
    receive
        #wx{userData = quitBtn} ->
            io:format("Button: ~p~n", [quitBtn]),
            unlink(ParentPid),
            exit(quitBtn);
        #wx{userData = spawnBtn} ->
            io:format("Button: ~p~n", [spawnBtn]),
            start(self());
        #wx{userData = errorBtn} ->
            io:format("Button: ~p~n", [errorBtn]),
            unlink(ParentPid),
            ParentPid ! {dead_child},
            error(errorBtn);
        {'EXIT', Pid, quitBtn} ->
            io:format("Process ~p quit.~n", [Pid]);
        {dead_child} ->
            start(self())
    end,
    loop(ParentPid).
%% ----------------------------------------------------------------------
%% Opens a window with three buttons for
%%  QUIT:   Closes window and all child windows
%%  SPAWN:  Spawns of a child window
%%  ERROR:  Dies because of an error, all children will also die.
%%          The window should be restarted by the parent window.
%% Title of the window will be the pid of the calling process.
%% The calling process will receive messages when a button is pressed.
%% Messages look like:
%% #wx{userData=ButtonName}
%%      where ButtonName is: quitBtn, spawnBtn or errorBtn
%% Returns: ok
%% ----------------------------------------------------------------------
make_window() ->
    W = wxFrame:new(wx:new(), ?wxID_ANY, pid_to_list(self())),
    P = wxPanel:new(W),
    Sz = wxBoxSizer:new(?wxHORIZONTAL),
    Q = wxButton:new(P, ?wxID_ANY, [{label, "Quit"}]),
    S = wxButton:new(P, ?wxID_ANY, [{label, "Spawn"}]),
    E = wxButton:new(P, ?wxID_ANY, [{label, "Error"}]),
    wxButton:setBackgroundColour(Q, {150, 250, 150}),
    wxButton:setBackgroundColour(S, {150, 150, 250}),
    wxButton:setBackgroundColour(E, {250, 150, 150}),
    wxSizer:add(Sz, Q),
    wxSizer:add(Sz, S),
    wxSizer:add(Sz, E),
    wxButton:connect(Q, command_button_clicked, [{userData, quitBtn}]),
    wxButton:connect(S, command_button_clicked, [{userData, spawnBtn}]),
    wxButton:connect(E, command_button_clicked, [{userData, errorBtn}]),
    wxPanel:setSizer(P, Sz),
    wxSizer:fit(Sz, W),
    wxFrame:show(W),
    ok.
