-module(demo).

-export([get_module_info/2]).

-define(BEAM_EXT, ".beam").

for_each_module(Fun, Dir) ->
    {ok, Filenames} = file:list_dir(Dir),
    [Fun(File) || File <- Filenames, string:rstr(File, ?BEAM_EXT) =/= 0].

get_module_info(Dir, Tag) ->
    GetInfo = fun(File) ->
        Stop = string:rstr(File, ?BEAM_EXT),
        FileName = list_to_atom(string:sub_string(File, 1, Stop - 1)),
        {FileName, app_name(File), FileName:module_info(list_to_atom(Tag))}
    end,
    for_each_module(GetInfo, Dir).

app_name(Dir) ->
    AbsName = filename:absname(Dir),
    case lists:reverse(filename:split(AbsName)) of
        [D, App | _] when D == "ebin"; D == "." -> App;
        ["..", _, App | _] -> App;
        [App | _] -> App
    end.
