% filename: my_esi.erl

-module(my_esi).
-export([start/0, stop/0, testValid/0, hasToken/1, track/3]).
-include_lib("inets/include/httpd.hrl"). 
-import(mochijson, [decode/1]).
-import(mochijson2, [json_string_is_safe/1]).

start() ->
	inets:start(),
	inets:start(httpd, [{port, 8080}, {server_name, "localhost"}, {server_root, "."}, {document_root, "."}, {modules,[mod_esi]}, {erl_script_alias, {"/esi", [my_esi, io]}}]).

stop() ->
	[inets:stop(element(1, X), element(2, X)) || X <- inets:services()],
	inets:stop().

testValid() ->
	isValid("data=ew0KICAgICJldmVudCI6ICJTaWduZWQgVXAiLA0KICAgICJwcm9wZXJ0aWVzIjogew0KICAgICAgICAiZGlzdGluY3RfaWQiOiAiMTM3OTMiLA0KICAgICAgICAidG9rZW4iOiAiZTNiYzQxMDAzMzBjMzU3MjI3NDBmYjhjNmY1YWJkZGMiLA0KICAgICAgICAiUmVmZXJyZWQgQnkiOiAiRnJpZW5kIg0KICAgIH0NCn0=").

hasToken(Data) ->
	case length(Data) /= 0 of
		true ->
			[Head|Tail] = Data,
			case element(1, Head) == "token" of
				true -> true;
				false -> hasToken(Tail)
			end;
		false -> false
	end.

% writeToFile(Request) ->
% 	file:write_file("./log.txt", io_lib:fwrite("~p.\n\n", [Request]), [append]).

% hasValidPrefix(Input) ->
% 	string:str(Input, "data=") == 1.

% hasEventParam(EventTuple) ->
% 	is_tuple(EventTuple) andalso tuple_size(EventTuple) == 2 andalso element(1, EventTuple) == "event";

% getOutermostTuple(Input) ->
% 	Base64Encoded = string:substr(Input, 6),
% 	Base64Decoded = base64:decode_to_string(Base64Encoded),
% 	mochijson:decode(Base64Decoded).

% getOutermostList(OutermostTuple) ->
% 	case is_tuple(OutermostTuple) andalso tuple_size(OutermostTuple) == 2 of
% 		true -> {_, Request}, Request;
% 		false -> false
% 	end.

% isValid2(Input) ->
% 	case hasValidPrefix(Input) andalso someFun() of
% 		true -> Result = "1", write


isValid(Input) ->
	case string:str(Input, "data=") == 1 of
		true ->
			Base64Encoded = string:substr(Input, 6),
			Base64Decoded = base64:decode_to_string(Base64Encoded),
			% json_string_is_safe doesn't accept valid JSON strings... so I'll just assume it's valid.
			%case mochijson2:json_string_is_safe(Base64Decoded) of
				%true ->
					Tuple = mochijson:decode(Base64Decoded),
					case is_tuple(Tuple) andalso tuple_size(Tuple) == 2 of
						true ->
							{_, Request} = Tuple,
							case is_list(Request) andalso length(Request) == 2 of
								true ->
									[EventTuple|PropTupleArr] = Request,
									case is_tuple(EventTuple) andalso tuple_size(EventTuple) == 2 andalso element(1, EventTuple) == "event" of
										true ->
											[PropTuple|_] = PropTupleArr,
											case is_tuple(PropTuple) andalso tuple_size(PropTuple) == 2 andalso element(1, PropTuple) == "properties" of
												true ->
													{_, Data} = PropTuple,
													case is_tuple(Data) andalso tuple_size(Data) == 2 of
														true ->
															{_, DataArr} = Data,
															case hasToken(DataArr) of
																true ->
																	Result = "1",
																	file:write_file("./log.txt", io_lib:fwrite("~p.\n\n", [Request]), [append]);
																false -> Result = "-8"
															end;
														false -> Result = "-7"
													end;
												false -> Result = "-6"
											end;
										false -> Result = "-5"
									end;
								false -> Result = "-4"
							end;
						false -> Result = "-3"
					end;
				%false -> Result = "-2"
			%end;
		false -> Result = "-1"
	end,
	Result.

track(SessionID, _Env, Input) ->
	case isValid(Input) == "1" of
		true -> Result = "1";
		false -> Result = "0"
	end,
	mod_esi:deliver(SessionID, Result).
