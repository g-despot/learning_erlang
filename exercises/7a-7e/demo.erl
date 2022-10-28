-module(demo).
-export([get_module_info/3, get_apps_data/1, search4func/1, save_apps_data/0, search4func_ets/1]).

-define(BEAM_EXT, ".beam").

%% 7A Apply a fun on module names
for_each_module(Fun, Dir, TagAtom) ->
    {ok, Filenames} = file:list_dir(Dir),
    [Fun(File, TagAtom) || File <- Filenames, string:rstr(File, ?BEAM_EXT) =/= 0].

for_each_module_ets(Fun, Dir, TagAtom) ->
    {ok, Filenames} = file:list_dir(Dir),
    [Fun(File, TagAtom) || File <- Filenames, string:rstr(File, ?BEAM_EXT) =/= 0].

%% 7B Read module info
get_module_info(Dir, Tag, Func) ->
    %% Make sure that Tag is a valid atom.
    if
        is_list(Tag) ->
            TagAtom = list_to_atom(Tag);
        true ->
            TagAtom = Tag
    end,
    for_each_module(Func, Dir, TagAtom).

get_info(File, TagAtom) ->
    Stop = string:rstr(File, ?BEAM_EXT),
    Mod = list_to_atom(string:sub_string(File, 1, Stop - 1)),
    {Mod, app_name(File), Mod:module_info(TagAtom)}.

%% This function was copied from the presentation and doesn't seem to be working correctly.
app_name(Dir) ->
    AbsName = filename:absname(Dir),
    case lists:reverse(filename:split(AbsName)) of
        [D, App | _] when D == "ebin"; D == "." -> App;
        ["..", _, App | _] -> App;
        [App | _] -> App
    end.

%% 7C Read module info data for all apps
get_apps_data(Tag) ->
    {PathsList, _} = lists:split(10, code:get_path()),
    % Paths = code:get_path(),    % This line was commented out to test the function faster.
    lists:flatten([get_module_info(Path, Tag, fun get_info/2) || Path <- PathsList]).

%% 7D Search for functions
search4func(FuncName) ->
    AppsList = get_apps_data("exports"),
    FoundListWithFalse = [search(FuncName, App) || App <- AppsList],
    FoundList = lists:filter(
        fun
            ({_, _}) -> true;
            (false) -> false
        end,
        FoundListWithFalse
    ),
    FoundList.

search(FuncName, {Module, App, ModuleInfo}) ->
    % io:format("Found: ~p", [ModuleInfo]),
    case lists:keyfind(FuncName, 1, ModuleInfo) of
        {_, _} ->
            % io:format("Found: ~p", [ModuleInfo]),
            {Module, App};
        false ->
            false
    end.

%% 7E Use ETS to cache data
save_apps_data() ->
    ets:new(functions, [set, named_table]),
    FuncList = get_apps_data_ets("exports"),
    lists:foreach(fun({Key, {Mod, App}}) -> ets:insert(functions, {Key, {Mod, App}}) end, FuncList).

get_apps_data_ets(Tag) ->
    {PathsList, _} = lists:split(10, code:get_path()),
    % Paths = code:get_path(),    % This line was commented out to test the function faster.
    lists:flatten([get_module_info(Path, Tag, fun get_info_ets/2) || Path <- PathsList]).

get_info_ets(File, TagAtom) ->
    Stop = string:rstr(File, ?BEAM_EXT),
    Mod = list_to_atom(string:sub_string(File, 1, Stop - 1)),
    [{Func, {Mod, app_name(File)}} || {Func, _} <- Mod:module_info(TagAtom)].

search4func_ets(FuncName) ->
    FoundList = ets:lookup(functions, FuncName),
    FoundList.
