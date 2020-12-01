-module(utils).

%% API
-export([echo1/2]).

-export([maps_get/2, maps_get/3, maps_put/3, maps_remove/2]).

-export([iso_timestamp/1, tdiff_seconds/2]).

-export([try_ets_lookup/2, try_ets_lookup/3]).

-export([calc_time/1]).

-export([urldecode/1, urlencode/1]).

-export([read_csv/1]).

-type map_path() :: [binary()] | binary().
-type timestamp() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type tid() :: integer().


%%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS    %%%
%%%%%%%%%%%%%%%%%%%%%%%

-spec echo1(iolist(), iolist()) -> ok.
echo1(NameArg1, Arg1) ->
	io:format("~n =============== ~n ~p : ~p ~n =============== ~n",
		[NameArg1, Arg1]).


-spec maps_get(map_path(), map()) -> Value :: any().
maps_get(Path, Map) ->
	GetFun = maps_getf(Path),
	GetFun(Map).

-spec maps_get(map_path(), map(), any()) -> Value :: any().
maps_get(Path, Map, Default) ->
	GetFun = maps_getf(Path, Default),
	GetFun(Map).


-spec maps_put(map_path(), any(), map()) -> Value :: any().
maps_put(Path, Value, Map) ->
	PutFun = maps_putf(Path),
	PutFun(Value, Map).


-spec maps_remove(map_path(), map()) -> map().
maps_remove(Path, Map) ->
	RemoveFun = maps_removef(Path),
	RemoveFun(Map).


-spec iso_timestamp(timestamp()) -> list().
iso_timestamp(TS) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(TS),
	lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])).


-spec tdiff_seconds(timestamp(), timestamp()) -> non_neg_integer().
tdiff_seconds(T1, T2) ->
	round(abs(timer:now_diff(T1, T2) / 1000000)).


-spec try_ets_lookup(Table :: tid(), Key :: binary()) -> Value :: any().
try_ets_lookup(Table, Key) ->
	try_ets_lookup(Table, Key, not_found).

-spec try_ets_lookup(Table :: tid(), Key :: binary(), Default :: any()) -> Value :: any().
try_ets_lookup(Table, Key, Default) ->
	case ets:lookup(Table, Key) of
		[{_, Val} | _] -> Val;
		[] -> Default;
		[Object | _] -> Object
	end.


calc_time(T1) ->
	T2 = erlang:monotonic_time(),
	erlang:convert_time_unit(T2 - T1, native, milli_seconds).


-spec urlencode(B) -> B when B::binary().
urlencode(B) ->
	urlencode(B, <<>>).


-spec urldecode(B) -> B when B::binary().
urldecode(B) ->
	urldecode(B, <<>>).


read_csv(File) ->
	try
		{ok, Bin} = file:read_file(File),
		{ok, parse(binary_to_list(Bin), [], [], [])}
	catch
		Class:Error ->
			{Class, Error}
	end.

parse([], _FBuff, _RBuff, Result) ->
	lists:reverse(Result);
parse([$" | Rest], _FBuff, RBuff, Result) ->
	{F, Rest1} = parse_q(Rest, []),
	parse(Rest1, [], [F | RBuff], Result);
parse([$,, $\s| Rest], FBuff, RBuff, Result) ->
	parse(Rest, [], [lists:reverse(FBuff) | RBuff], Result);
parse([$, | Rest], FBuff, RBuff, Result) ->
	parse(Rest, [], [lists:reverse(FBuff) | RBuff], Result);
parse([$\r, $\n | Rest], FBuff, RBuff, Result) ->
	parse(Rest, [], [], [lists:reverse([lists:reverse(FBuff) | RBuff]) | Result]);
parse([$\n | Rest], FBuff, RBuff, Result) ->
	parse(Rest, [], [], [lists:reverse([lists:reverse(FBuff) | RBuff]) | Result]);
parse([A | Rest], FBuff, RBuff, Result) ->
	parse(Rest, [A | FBuff], RBuff, Result).

parse_q([$", $, | Rest], Result) ->
	{lists:reverse(Result), Rest};
parse_q([A | Rest], Result) ->
	parse_q(Rest, [A | Result]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

maps_getf(Path) ->
	fun(Map) ->
		maps_getf_internal(Path, Map) end.

maps_getf_internal([Key | _PathRest], Map) when is_list(Key)->
	lists:foldl(
		fun(Map2, Acc) ->
			Acc ++ [maps_getf_internal(Key, Map2)]
		end
		,[], Map);
maps_getf_internal([Key | PathRest], Map) ->
	try maps:get(Key, Map) of
		Value ->
			maps_getf_internal(PathRest, Value)
	catch
		_:_ ->
			{error, bad_key}
	end;
maps_getf_internal([], Value) ->
	Value.


maps_getf(Path, Default) ->
	fun(Map) ->
		maps_getf_internal(Path, Map, Default) end.

maps_getf_internal([Key | PathRest], Map, Default) ->
	try maps:get(Key, Map, Default) of
		Value ->
			maps_getf_internal(PathRest, Value, Default)
	catch
		_:_ ->
			Default
	end;
maps_getf_internal([], Value, _) ->
	Value.


maps_putf(Path) ->
	fun(Value, Map) ->
		maps_putf_internal(Path, Value, Map) end.

maps_putf_internal([Key | PathRest], Value, Map) ->
	SubMap =
		case maps:is_key(Key, Map) andalso is_map(maps:get(Key, Map)) of
			true ->
				maps:get(Key, Map);
			false ->
				#{}
		end,
	maps:put(Key, maps_putf_internal(PathRest, Value, SubMap), Map);
maps_putf_internal([], Value, _) ->
	Value.


maps_removef(Path) ->
	fun(Map) ->
		maps_removef_internal(Path, Map) end.

maps_removef_internal([], _) ->
	throw({bad_path, []});
maps_removef_internal([LastKey], Map) ->
	maps:remove(LastKey, Map);
maps_removef_internal([Key | PathRest], Map) ->
	case maps:is_key(Key, Map) of
		true ->
			maps:put(Key, maps_removef_internal(PathRest, maps:get(Key, Map)), Map);
		false ->
			Map
	end.


urlencode(<< $\s, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $+ >>);
urlencode(<< $-, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $- >>);
urlencode(<< $., Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $. >>);
urlencode(<< $0, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $0 >>);
urlencode(<< $1, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $1 >>);
urlencode(<< $2, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $2 >>);
urlencode(<< $3, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $3 >>);
urlencode(<< $4, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $4 >>);
urlencode(<< $5, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $5 >>);
urlencode(<< $6, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $6 >>);
urlencode(<< $7, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $7 >>);
urlencode(<< $8, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $8 >>);
urlencode(<< $9, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $9 >>);
urlencode(<< $A, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $A >>);
urlencode(<< $B, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $B >>);
urlencode(<< $C, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $C >>);
urlencode(<< $D, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $D >>);
urlencode(<< $E, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $E >>);
urlencode(<< $F, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $F >>);
urlencode(<< $G, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $G >>);
urlencode(<< $H, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $H >>);
urlencode(<< $I, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $I >>);
urlencode(<< $J, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $J >>);
urlencode(<< $K, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $K >>);
urlencode(<< $L, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $L >>);
urlencode(<< $M, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $M >>);
urlencode(<< $N, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $N >>);
urlencode(<< $O, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $O >>);
urlencode(<< $P, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $P >>);
urlencode(<< $Q, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $Q >>);
urlencode(<< $R, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $R >>);
urlencode(<< $S, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $S >>);
urlencode(<< $T, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $T >>);
urlencode(<< $U, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $U >>);
urlencode(<< $V, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $V >>);
urlencode(<< $W, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $W >>);
urlencode(<< $X, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $X >>);
urlencode(<< $Y, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $Y >>);
urlencode(<< $Z, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $Z >>);
urlencode(<< $_, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $_ >>);
urlencode(<< $a, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $a >>);
urlencode(<< $b, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $b >>);
urlencode(<< $c, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $c >>);
urlencode(<< $d, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $d >>);
urlencode(<< $e, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $e >>);
urlencode(<< $f, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $f >>);
urlencode(<< $g, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $g >>);
urlencode(<< $h, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $h >>);
urlencode(<< $i, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $i >>);
urlencode(<< $j, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $j >>);
urlencode(<< $k, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $k >>);
urlencode(<< $l, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $l >>);
urlencode(<< $m, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $m >>);
urlencode(<< $n, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $n >>);
urlencode(<< $o, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $o >>);
urlencode(<< $p, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $p >>);
urlencode(<< $q, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $q >>);
urlencode(<< $r, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $r >>);
urlencode(<< $s, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $s >>);
urlencode(<< $t, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $t >>);
urlencode(<< $u, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $u >>);
urlencode(<< $v, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $v >>);
urlencode(<< $w, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $w >>);
urlencode(<< $x, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $x >>);
urlencode(<< $y, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $y >>);
urlencode(<< $z, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $z >>);
urlencode(<< C, Rest/bits >>, Acc) ->
	H = hex(C bsr 4),
	L = hex(C band 16#0f),
	urlencode(Rest, << Acc/bits, $%, H, L >>);
urlencode(<<>>, Acc) ->
	Acc.

hex( 0) -> $0;
hex( 1) -> $1;
hex( 2) -> $2;
hex( 3) -> $3;
hex( 4) -> $4;
hex( 5) -> $5;
hex( 6) -> $6;
hex( 7) -> $7;
hex( 8) -> $8;
hex( 9) -> $9;
hex(10) -> $A;
hex(11) -> $B;
hex(12) -> $C;
hex(13) -> $D;
hex(14) -> $E;
hex(15) -> $F.


urldecode(<< $%, H, L, Rest/bits >>, Acc) ->
	C = (unhex(H) bsl 4 bor unhex(L)),
	urldecode(Rest, << Acc/bits, C >>);
urldecode(<< $+, Rest/bits >>, Acc) ->
	urldecode(Rest, << Acc/bits, " " >>);
urldecode(<< C, Rest/bits >>, Acc) when C =/= $% ->
	urldecode(Rest, << Acc/bits, C >>);
urldecode(<<>>, Acc) ->
	Acc.

unhex($0) ->  0;
unhex($1) ->  1;
unhex($2) ->  2;
unhex($3) ->  3;
unhex($4) ->  4;
unhex($5) ->  5;
unhex($6) ->  6;
unhex($7) ->  7;
unhex($8) ->  8;
unhex($9) ->  9;
unhex($A) -> 10;
unhex($B) -> 11;
unhex($C) -> 12;
unhex($D) -> 13;
unhex($E) -> 14;
unhex($F) -> 15;
unhex($a) -> 10;
unhex($b) -> 11;
unhex($c) -> 12;
unhex($d) -> 13;
unhex($e) -> 14;
unhex($f) -> 15.

