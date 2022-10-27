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

-include("includes/definitions.hrl").

start() ->
    gen_server:start_link({global, ?WM_CTRL_SERVER}, ?WM_CTRL_SERVER, [], []).

init([]) ->
    ets:new(wm_table, [set, named_table]),
    {ok, #state{}}.

handle_call(Request, _From, State) ->
    case Request of
        {add} ->
            start_wm();
        {register, Pid} ->
            Id = register(Pid),
            wm_logger:log("PID: ~p ID: ~p", [Pid, Id]);
        {remove, Id} ->
            remove_wm(Id);
        {status, Id} ->
            get_status(Id);
        {remote_ctrl, Id, Action} ->
            remote_ctrl(Id, Action);
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
    ets:insert(wm_table, {Id, Pid}),
    wm_logger:log("REGISTERED PID: ~p ID: ~p", [Pid, Id]).

remove_wm(Id) ->
    ets:take(wm_table, Id).

get_status(Id) ->
    [{Id, Pid}] = ets:lookup(wm_table, Id),
    wm_logger:log("GET STATUS FOR PID: ~p", [Pid]),
    Pid ! {where_you_at_sig}.

remote_ctrl(Id, Action) ->
    [{Id, Pid}] = ets:lookup(wm_table, Id),
    Pid ! Action.

stop() ->
    ok.
