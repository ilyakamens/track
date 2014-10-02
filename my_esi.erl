% filename: my_esi.erl

-module(my_esi).
-export([start/0, stop/0, testValid/0, track/3]).
-include_lib("inets/include/httpd.hrl"). 
-import(mochijson, [decode/1]).

-define(SUCCESS, "1").
-define(FAILURE, "0").
-define(PROPERTIES_KEY, "properties").
-define(TOKEN_KEY, "token").
-define(EVENT_KEY, "event").
-define(DATA_PREFIX, "data=").
-define(IP_PREFIX, "ip=").
-define(REDIRECT_PREFIX, "redirect=").
-define(IMG_PREFIX, "img=").
-define(CALLBACK_PREFIX, "callback=").
-define(VERBOSE_PREFIX, "verbose=").
-define(OPTIONAL_PREFIXES, [?IP_PREFIX, ?REDIRECT_PREFIX, ?IMG_PREFIX, ?CALLBACK_PREFIX, ?VERBOSE_PREFIX]).

start() ->
	inets:start(),
	inets:start(httpd, [{port, 8080}, {server_name, "localhost"}, {server_root, "."}, {document_root, "."}, {modules,[mod_esi]}, {erl_script_alias, {"/esi", [my_esi, io]}}]).

stop() ->
	[inets:stop(element(1, X), element(2, X)) || X <- inets:services()],
	inets:stop().

testValid() -> isValid("data=ew0KICAgICJldmVudCI6ICJTaWduZWQgVXAiLA0KICAgICJwcm9wZXJ0aWVzIjogew0KICAgICAgICAiZGlzdGluY3RfaWQiOiAiMTM3OTMiLA0KICAgICAgICAidG9rZW4iOiAiZTNiYzQxMDAzMzBjMzU3MjI3NDBmYjhjNmY1YWJkZGMiLA0KICAgICAgICAiUmVmZXJyZWQgQnkiOiAiRnJpZW5kIg0KICAgIH0NCn0=").

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

writeToFile(Request) -> file:write_file("./log.txt", io_lib:fwrite("~p.\n\n", [Request]), [append]).

getOutermostTuple(Input) ->
	Base64Encoded = string:substr(Input, string:str(Input, ?DATA_PREFIX) + string:len(?DATA_PREFIX)),
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
	case is_tuple(PropTuple) andalso tuple_size(PropTuple) == 2 of
		true -> {_, DataTuple} = PropTuple, DataTuple;
		false -> false
	end.

getDataList(DataTuple) ->
	case is_tuple(DataTuple) andalso tuple_size(DataTuple) == 2 of
		true -> {_, DataList} = DataTuple, DataList;
		false -> false
	end.

getSeparatedParameters(Input) -> string:tokens(Input, "&").

removePrefix(Input, Prefix) ->
	string:substr(Input, string:str(Input, Prefix) + string:len(Prefix)).

getOptionalResponse([]) -> ?SUCCESS;
getOptionalResponse(List) ->
	[Option|_] = List,
	getOptionalResponse(Option, ?OPTIONAL_PREFIXES).

getOptionalResponse(_Option, []) -> ?SUCCESS;
getOptionalResponse(IP, ?IP_PREFIX) -> IP;
getOptionalResponse(Redirect, ?REDIRECT_PREFIX) -> Redirect;
getOptionalResponse(Img, ?IMG_PREFIX) -> Img;
getOptionalResponse(Callback, ?CALLBACK_PREFIX) -> Callback;
getOptionalResponse(Verbose, ?VERBOSE_PREFIX) -> Verbose;
getOptionalResponse(Option, Prefixes) ->
	[Prefix|Tail] = Prefixes,
	case string:str(Option, Prefix) == 1 of
		true -> getOptionalResponse(removePrefix(Option, Prefix), Prefix);
		false -> getOptionalResponse(Option, Tail)
	end.

isValid(Input) ->
	[Data|Optional] = getSeparatedParameters(Input),
	OutermostTuple = getOutermostTuple(Data),
	OutermostList = getOutermostList(OutermostTuple),
	EventTuple = getEventTuple(OutermostList),
	PropertiesTuple = getPropertiesTuple(OutermostList),
	DataList = getDataList(getDataTuple(PropertiesTuple)),
	case hasKey(EventTuple, ?EVENT_KEY) andalso hasKey(PropertiesTuple, ?PROPERTIES_KEY) andalso hasKey(DataList, ?TOKEN_KEY) of
		true -> writeToFile(OutermostTuple), getOptionalResponse(Optional);
		false -> ?FAILURE
	end.

track(SessionID, _Env, Input) ->
	Result = isValid(Input),
	mod_esi:deliver(SessionID, Result).
