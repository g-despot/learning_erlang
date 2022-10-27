-module(lcs_server).
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
    {ok, _Pid} = gen_event:start_link({local, ?WM_EVENT_MANAGER}),
    gen_event:add_handler(?WM_EVENT_MANAGER, ?WM_EVENT_HANDLER, []),
    gen_server:start_link({global, ?LCS_SERVER}, ?LCS_SERVER, [], []).

init([]) ->
    ets:new(accounts_table, [set, named_table]),
    start_wm_controller_operator(),
    {ok, ready}.

handle_call(Request, _From, State) ->
    case Request of
        {register_acc, Username, Password} ->
            wm_logger:log("Registering account..."),
            register_acc(Username, Password);
        {sign_in, Username, Password} ->
            wm_logger:log("Signing in..."),
            sign_in(Username, Password);
        {sign_out} ->
            wm_logger:log("Signing out..."),
            sign_out();
        {book_wm} ->
            book_wm();
        {remote_ctrl_wm, WMId, WMAction} ->
            remote_ctrl_wm(WMId, WMAction);
        {stop} ->
            stop();
        _Other ->
            wm_logger:log("WTF")
    end,
    {reply, State, State}.

handle_cast(_Msg, _State) ->
    not_implemented.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start_wm_controller_operator() ->
    spawn(?WM_CTRL_OPERATOR, start, []).

register_acc(Username, Password) ->
    ets:insert(accounts_table, {Username, crypto:hash(md5, Password)}),
    wm_logger:log("ETS_INSERT USERNAME: ~p PASSWORD: ~p", [Username, Password]).

sign_in(Username, InputPassword) ->
    [{Username, HashedPassword}] = ets:lookup(accounts_table, Username),
    case crypto:hash(md5, InputPassword) of
        HashedPassword ->
            wm_logger:log("LOGGED IN: ~p", [Username]);
        _Other ->
            wm_logger:log("WRONG PASSWORD: ~p", [InputPassword])
    end.

sign_out() ->
    ok.

book_wm() ->
    % Should be a message/cast/call and not a function call
    ?WM_CTRL_OPERATOR:book_wm(self()),
    receive
        {WMId, booked} -> wm_logger:log("BOOKED WM: ~p", [WMId])
    end,
    WMId.

remote_ctrl_wm(WMId, WMAction) ->
    % Should be a message/cast/call and not a function call
    ?WM_CTRL_OPERATOR:remote_ctrl(WMId, WMAction).

stop() ->
    ok.
