-module(wm_logger).
-behaviour(gen_event).

-export([
    code_change/3,
    handle_call/2,
    handle_event/2,
    handle_info/2,
    init/1,
    terminate/2
]).
-export([log/1, log/2]).

-include("includes/definitions.hrl").

init(_Args) ->
    {ok, 0}.

handle_event({Message, Args}, Count) ->
    io:format(Message ++ "~n~n", Args),
    {ok, Count + 1}.

handle_call({Message, Args}, Count) ->
    io:format(Message ++ "~n~n", Args),
    {ok, Count, Count}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Args, _State) -> ok.

log(Message) ->
    gen_event:notify(?WM_EVENT_MANAGER, {Message, []}).
log(Message, Args) ->
    gen_event:notify(?WM_EVENT_MANAGER, {Message, Args}).
