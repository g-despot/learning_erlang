-module(xe).

-export([print_rates/1, convert/3]).
-import(xml_extract, [get_currency/1]).
-import(rss_xe, [rate_string_to_num/1]).

-include_lib("xmerl/include/xmerl.hrl").

-record(xe_info, {currency, date, rate}).

print_rates(Currency) ->
    Rates = get_currency(Currency),
    lists:foreach(fun print_6d/1, Rates),
    lists:map(fun create_records_6e/1, Rates).

print_6d([TitleRecord, _, DescrRecord]) ->
    [TitleText] = TitleRecord#xmlElement.content,
    [DescrText] = DescrRecord#xmlElement.content,
    io:format("~s exchange rate: ~s~n", [TitleText#xmlText.value, DescrText#xmlText.value]).

create_records_6e([TitleRecord, PubDateRecord, DescrRecord]) ->
    [TitleText] = TitleRecord#xmlElement.content,
    [PubDateText] = PubDateRecord#xmlElement.content,
    [DescrText] = DescrRecord#xmlElement.content,
    DescrShort = rss_xe:rate_string_to_num(DescrText#xmlText.value),
    #xe_info{
        currency = TitleText#xmlText.value, date = PubDateText#xmlText.value, rate = DescrShort
    }.

convert(FromCurr, ToCurr, Amount) ->
    XeInfoList = print_rates(FromCurr),
    Key = string:uppercase(atom_to_list(ToCurr)) ++ "/" ++ string:uppercase(atom_to_list(FromCurr)),
    {value, Tuple} = lists:keysearch(Key, #xe_info.currency, XeInfoList),
    Converted = Amount * Tuple#xe_info.rate,
    io:format("~s ~p = ~p ~s~n", [FromCurr, Amount, Converted, ToCurr]).
