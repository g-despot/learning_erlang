-module(wm_controller_server).
-behaviour(gen_server).

-export([
    start/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).
-export([start_up_wms/1]).

-include("includes/definitions.hrl").

start() ->
    gen_server:start_link({global, ?WM_CTRL_SERVER}, ?WM_CTRL_SERVER, [], []).

%% Create an ETS table to store Id <=> Pid WM relationships.
%% The func start_up_wm defines how many WMs should be added right on start.
init([]) ->
    ets:new(wm_table, [set, named_table]),
    State = #wm_ctrl_state{},
    #wm_ctrl_state{start_up_wm = N} = State,
    start_up_wms(N),
    {ok, State}.

handle_call(Request, _From, State) ->
    case Request of
        {add} ->
            start_wm();
        {register, Pid} ->
            Id = register(Pid),
            wm_logger:log("PID: ~p ID: ~p", [Pid, Id]);
        {remove, Id} ->
            remove_wm(Id);
        {status} ->
            get_status();
        {status, Id} ->
            get_status(Id);
        {remote_ctrl, Id, Action} ->
            remote_ctrl(Id, Action);
        {FromPid, book_wm} ->
            get_free_wm(FromPid);
        {stop} ->
            stop();
        _Other ->
            wm_logger:log("WTF")
    end,
    {reply, finished, State}.

handle_cast(_Msg, _State) ->
    not_implemented.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start_wm() ->
    spawn(?WM_OPERATOR, start, []).

register(Pid) ->
    Id = erlang:unique_integer([monotonic, positive]),
    ets:insert(wm_table, {Id, Pid, free}),
    wm_logger:log("REGISTERED PID: ~p ID: ~p", [Pid, Id]).

remove_wm(Id) ->
    ets:take(wm_table, Id).

get_status() ->
    [{size, Size}] = ets:info(wm_table),
    wm_logger:log("GET STATUS - ACTIVE_WMS: ", [Size]),
    Size.

get_status(Id) ->
    [{Id, Pid, _Booked}] = ets:lookup(wm_table, Id),
    wm_logger:log("GET STATUS FOR WM PID: ~p", [Pid]),
    Pid ! {self(), where_you_at_sig},
    %{Status} = gen_statem:call({global, ?WM_OPERATOR}, {self(), where_you_at_sig}),
    receive
        {Status} -> ok
    end,
    Status.

get_free_wm(FromPid) when is_pid(FromPid) ->
    wm_logger:log("FINDING FREE WM"),
    WMList = ets:tab2list(wm_table),
    get_free_wm(WMList, WMList, FromPid).

%% This should be reimplemented with async in mind but wanted to keep it simple.
get_free_wm([{Id, Pid, _Booked} | T], WMList, FromPid) ->
    wm_logger:log("MACHINE STATUS: ~p", [get_status(Id)]),
    case idle =:= get_status(Id) of
        true ->
            ets:insert(wm_table, {Id, Pid, booked}),
            FromPid ! {Id, booked},
            get_free_wm([], [], FromPid);
        false ->
            get_free_wm(T, WMList, FromPid)
    end;
get_free_wm([], WMList, FromPid) when length(WMList) > 0 ->
    WMList = ets:tab2list(wm_table),
    get_free_wm(WMList, WMList, FromPid);
get_free_wm([], [], _FromPid) ->
    ok.

remote_ctrl(Id, Action) ->
    [{Id, Pid, _Booked}] = ets:lookup(wm_table, Id),
    wm_logger:log("ACTION: ~p ID: ~p PID: ~p", [Action, Id, Pid]),
    Pid ! Action.

%% TODO: Implement a graceful exit strategy and handle error exits.
stop() ->
    not_implemented.

start_up_wms(N) ->
    [start_wm() || _X <- lists:seq(1, N)],
    wm_logger:log("STARTED ~p WASHING MACHINES", [N]).
