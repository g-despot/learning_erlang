-module(lcs_operator).

-export([test/0, start/0, register_acc/2, stop/0]).

-include("includes/definitions.hrl").

start() ->
    ?LCS_SERVER:start().

test() ->
    server_call({test}).

register_acc(Username, Password) ->
    server_call({register_acc, Username, Password}).

stop() ->
    server_call({stop}).

server_call(Message) ->
    gen_server:call({global, ?LCS_SERVER}, Message).
