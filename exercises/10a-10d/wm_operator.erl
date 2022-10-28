-module(wm_operator).
-behaviour(gen_statem).

-export([start/0, init/1, callback_mode/0, terminate/3, code_change/4]).
-export([temperature/1, quick_programme/1, prewash/1, dry/1]).
-export([start_wm/0, skip/0, cancel/0, where_you_at/0]).
-export([idle/3, prewash/3, main_wash/3, rinse/3, dry/3]).

-include("includes/definitions.hrl").

start() ->
    ?WM_SERVER:start(),
    {ok, Pid} = gen_statem:start_link(?WM_OPERATOR, [], []),
    ctrl_server_call({register, Pid}).

init(_Args) ->
    Data = #{temperature => 30, quick_programme => off, prewash => off, dry => off},
    {ok, idle, Data}.

idle(_, Message, #{prewash := PreWash} = Data) ->
    case Message of
        {start_sig} ->
            if
                PreWash =:= on ->
                    server_cast(prewash),
                    {next_state, prewash, Data};
                PreWash =:= off ->
                    server_cast(main_wash),
                    {next_state, main_wash, Data}
            end;
        {FromPid, where_you_at_sig} ->
            FromPid ! {idle},
            wm_logger:log("STATE: idle"),
            {next_state, idle, Data};
        {Option, Value} ->
            {next_state, idle, Data#{Option := Value}};
        _Wrong ->
            {next_state, idle, Data}
    end.

prewash(_, Message, Data) ->
    case Message of
        {finished_sig} ->
            server_cast(main_wash),
            {next_state, main_wash, Data};
        {end_sig} ->
            {next_state, idle, Data};
        {skip_sig} ->
            server_cast(main_wash),
            {next_state, main_wash, Data};
        {FromPid, where_you_at_sig} ->
            FromPid ! {prewash},
            wm_logger:log("STATE: prewash"),
            {next_state, prewash, Data};
        _Wrong ->
            {next_state, prewash, Data}
    end.

main_wash(_, Message, #{quick_programme := _QuickProg} = Data) ->
    case Message of
        {finished_sig} ->
            server_cast(rinse),
            {next_state, rinse, Data};
        {end_sig} ->
            {next_state, idle, Data};
        {skip_sig} ->
            server_cast(rinse),
            {next_state, rinse, Data};
        {FromPid, where_you_at_sig} ->
            FromPid ! {main_wash},
            wm_logger:log("STATE: main_wash"),
            {next_state, main_wash, Data};
        _Wrong ->
            {next_state, main_wash, Data}
    end.

rinse(_, Message, #{dry := Dry} = Data) ->
    case Message of
        {finished_sig} ->
            if
                Dry =:= on ->
                    server_cast(dry),
                    {next_state, dry, Data};
                Dry =:= off ->
                    {next_state, idle, Data}
            end;
        {end_sig} ->
            {next_state, idle, Data};
        {skip_sig} ->
            server_cast(dry),
            {next_state, dry, Data};
        {FromPid, where_you_at_sig} ->
            FromPid ! {rinse},
            wm_logger:log("STATE: rinse"),
            {next_state, rinse, Data};
        _Wrong ->
            {next_state, rinse, Data}
    end.

dry(_, Message, Data) ->
    case Message of
        {finished_sig} ->
            {next_state, idle, Data};
        {end_sig} ->
            {next_state, idle, Data};
        {FromPid, where_you_at_sig} ->
            FromPid ! {dry},
            wm_logger:log("STATE: dry"),
            {next_state, dry, Data};
        _Wrong ->
            {next_state, dry, Data}
    end.

callback_mode() ->
    state_functions.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

temperature(Temp) ->
    statem_cast({temperature, Temp}).

quick_programme(QuickProg) ->
    statem_cast({quick_programme, QuickProg}).

prewash(PreWash) ->
    statem_cast({prewash, PreWash}).

dry(Dry) ->
    statem_cast({dry, Dry}).

start_wm() ->
    statem_cast({start_sig}).

skip() ->
    statem_cast({skip_sig}).

cancel() ->
    statem_cast({cancel_sig}).

where_you_at() ->
    statem_cast({where_you_at_sig}).

statem_cast(Message) ->
    gen_statem:cast({global, ?WM_OPERATOR}, Message).

server_cast(Message) ->
    gen_server:cast({global, ?WM_SERVER}, {self(), Message}).

ctrl_server_call(Message) ->
    gen_server:call({global, ?WM_CTRL_SERVER}, Message).

