-module(wm_controller_operator).

-export([start/0, add/0, remove/1, status/1, remote_ctrl/2, book_wm/1, stop/0]).

-include("includes/definitions.hrl").

start() ->
    ?WM_CTRL_SERVER:start().

add() ->
    server_call({add}).

remove(Id) ->
    server_call({remove, Id}).

status(Id) ->
    server_call({status, Id}).

remote_ctrl(Id, Action) ->
    server_call({remote_ctrl, Id, Action}).

book_wm(FromPid) ->
    server_call({FromPid, book_wm}).

stop() ->
    server_call({stop}).

server_call(Message) ->
    gen_server:call({global, ?WM_CTRL_SERVER}, Message).
