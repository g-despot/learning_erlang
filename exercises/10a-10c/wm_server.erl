-module(wm_server).
-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2]).

-include("includes/definitions.hrl").

start() ->
    gen_server:start_link({local, ?WM_SERVER}, ?WM_SERVER, [], []).

init(_Args) ->
    {ok, idle}.

handle_cast(Request, _State) ->
    case Request of
        {Pid, prewash} ->
            wm_logger:log("Server prewashing..."),
            act_like_a_washing_machine(3000),
            reply(Pid, finished_sig),
            {noreply, prewash};
        {Pid, main_wash} ->
            wm_logger:log("Server washing..."),
            act_like_a_washing_machine(3000),
            reply(Pid, finished_sig),
            {noreply, main_wash};
        {Pid, dry} ->
            wm_logger:log("Server drying..."),
            act_like_a_washing_machine(3000),
            reply(Pid, finished_sig),
            {noreply, dry};
        {Pid, rinse} ->
            wm_logger:log("Server rinsing... "),
            act_like_a_washing_machine(3000),
            reply(Pid, finished_sig),
            {noreply, rinse}
    end.

%% Only async communication is acceptable for this exercise.
handle_call(_Request, _From, _State) ->
    ok.

reply(Pid, Message) ->
    Pid ! {Message}.

%% Simulating typical washing machine behaviour.
act_like_a_washing_machine(Time) ->
    timer:sleep(Time).
