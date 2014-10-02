% filename: my_esi.erl

-module(my_esi).
-export([start/0, stop/0, testValid/0, track/3]).
-include_lib("inets/include/httpd.hrl"). 
-import(mochijson, [decode/1]).

start() ->
	inets:start(),
	inets:start(httpd, [{port, 8080}, {server_name, "localhost"}, {server_root, "."}, {document_root, "."}, {modules,[mod_esi]}, {erl_script_alias, {"/esi", [my_esi, io]}}]).

stop() ->
	[inets:stop(element(1, X), element(2, X)) || X <- inets:services()],
	inets:stop().

testValid() ->
	isValid("data=ew0KICAgICJldmVudCI6ICJTaWduZWQgVXAiLA0KICAgICJwcm9wZXJ0aWVzIjogew0KICAgICAgICAiZGlzdGluY3RfaWQiOiAiMTM3OTMiLA0KICAgICAgICAidG9rZW4iOiAiZTNiYzQxMDAzMzBjMzU3MjI3NDBmYjhjNmY1YWJkZGMiLA0KICAgICAgICAiUmVmZXJyZWQgQnkiOiAiRnJpZW5kIg0KICAgIH0NCn0=").

hasKey(List, Key) when is_list(List) ->
	case length(List) /= 0 of
		true ->
			[Head|Tail] = List,
			case hasKey(Head, Key) of
				true -> true;
				false -> hasKey(Tail, Key)
			end;
		false -> false
	end;
hasKey(Tuple, Key) when is_tuple(Tuple) ->
	tuple_size(Tuple) == 2 andalso element(1, Tuple) == Key;
hasKey(false, _Key) ->
	false.

writeToFile(Request) ->
	file:write_file("./log.txt", io_lib:fwrite("~p.\n\n", [Request]), [append]).

hasValidPrefix(Input) ->
	string:str(Input, "data=") == 1.

getOutermostTuple(Input) ->
	Base64Encoded = string:substr(Input, 6),
	try
		Base64Decoded = base64:decode_to_string(Base64Encoded),
		mochijson:decode(Base64Decoded)
	catch
		_:_ -> false
	end.

getOutermostList(OutermostTuple) ->
	case is_tuple(OutermostTuple) andalso tuple_size(OutermostTuple) == 2 of
		true -> {_, Request} = OutermostTuple, Request;
		false -> false
	end.

getEventTuple(OutermostList) ->
	case is_list(OutermostList) andalso length(OutermostList) == 2 of
		true -> [EventTuple|_] = OutermostList, EventTuple;
		false -> false
	end.

getPropertiesTuple(OutermostList) ->
	case is_list(OutermostList) andalso length(OutermostList) == 2 of
		true ->
			[_|OuterPropList] = OutermostList,
			[PropTuple|_] = OuterPropList,
			PropTuple;
		false -> false
	end.

getDataTuple(PropTuple) ->
	case is_tuple(PropTuple) andalso tuple_size(PropTuple) == 2 andalso element(1, PropTuple) == "properties" of
		true -> {_, DataTuple} = PropTuple, DataTuple;
		false -> false
	end.

getDataList(DataTuple) ->
	case is_tuple(DataTuple) andalso tuple_size(DataTuple) == 2 of
		true -> {_, DataList} = DataTuple, DataList;
		false -> false
	end.

isValid(Input) ->
	case hasValidPrefix(Input) of
		true ->
			OutermostTuple = getOutermostTuple(Input),
			OutermostList = getOutermostList(OutermostTuple),
			EventTuple = getEventTuple(OutermostList),
			PropertiesTuple = getPropertiesTuple(OutermostList),
			DataList = getDataList(getDataTuple(PropertiesTuple)),
			case hasKey(EventTuple, "event") andalso hasKey(PropertiesTuple, "properties") andalso hasKey(DataList, "token") of
				true -> writeToFile(OutermostTuple), "1";
				false -> "0"
			end;
		false -> "0"
	end.

track(SessionID, _Env, Input) ->
	Result = isValid(Input),
	mod_esi:deliver(SessionID, Result).
