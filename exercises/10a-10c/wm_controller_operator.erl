-module(wm_controller_operator).

-export([start/0, add/0, remove/1, status/1, remote_ctrl/2, stop/0]).

-include("includes/definitions.hrl").

start() ->
    {ok, _Pid} = gen_event:start_link({local, ?WM_EVENT_MANAGER}),
    gen_event:add_handler(?WM_EVENT_MANAGER, ?WM_EVENT_HANDLER, []),

    ?WM_CTRL_SERVER:start().

add() ->
    server_call({add}).

remove(Id) ->
    server_call({remove, Id}).

status(Id) ->
    server_call({status, Id}).

remote_ctrl(Id, Action) ->
    server_call({remote_ctrl, Id, Action}).

stop() ->
    server_call({stop}).

server_call(Message) ->
    gen_server:call(?WM_CTRL_SERVER, Message).
