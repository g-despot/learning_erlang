-module(lcs_operator).

-export([
    test/0, start/0, register_acc/2, sign_in/2, sign_out/0, book_wm/0, remote_ctrl_wm/2, stop/0
]).

-include("includes/definitions.hrl").

start() ->
    ?LCS_SERVER:start().

test() ->
    server_call({test}).

register_acc(Username, Password) ->
    server_call({register_acc, Username, Password}).

sign_in(Username, Password) ->
    server_call({sign_in, Username, Password}).

sign_out() ->
    server_call({sign_out}).

book_wm() ->
    server_call({book_wm}).

remote_ctrl_wm(WMId, WMAction) ->
    server_call({remote_ctrl_wm, WMId, WMAction}).

stop() ->
    server_call({stop}).

server_call(Message) ->
    gen_server:call({global, ?LCS_SERVER}, Message).
