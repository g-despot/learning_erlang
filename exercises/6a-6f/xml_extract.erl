-module(xml_extract).
-export([get_currency/1]).

-include_lib("xmerl/include/xmerl.hrl").

filter_by_element(Element) ->
    case Element#xmlElement.name of
        title -> true;
        description -> true;
        pubDate -> true;
        _ -> false
    end.

get_currency(Currency) when is_atom(Currency) ->
    LowercaseCurrencies = ["usd", "sek"],
    CurrencyLowercaseString = string:lowercase(atom_to_list(Currency)),
    io:format("You selected: ~p~n", [CurrencyLowercaseString]),
    case lists:member(CurrencyLowercaseString, LowercaseCurrencies) of
        true ->
            get_elements(
                "./xmls/" ++ string:uppercase(CurrencyLowercaseString) ++
                    ".xml"
            );
        false ->
            io:format("Invalid currency.~n", [])
    end.

get_elements(FilePath) ->
    Records = xmlget:file2rec(FilePath),
    rss_xe:get_xe_info(Records, fun filter_by_element/1).
