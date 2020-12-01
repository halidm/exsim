-module(exsim).

-behaviour(gen_server).

-include("global.hrl").
-include("lager.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([get_exchange_info/0, get_candles/1, get_tick/1]).


-export([start_link/0, reset/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



-ifdef(TEST).
-compile(export_all).
-endif.


-record(state, {
	tick,
	tick_counter = 0, % only at init tick_counter can be 0, afterwards loops from 1
	candle,
	interval
}).

-record(candle, {
	timestamp,
	open,
	high,
	low,
	close,
	volume,
	close_timestamp,
	quote_volume,
	number,
	taker_base,
	taker_quote,
	ignore
}).

-record(tick, {
	symbol = <<"ETHBTC">>,
	priceChange = <<"0.2">>,
	priceChangePercent = <<"4.4">>,
	weightedAvgPrice,
	prevClosePrice = <<"2.3">>,
	lastPrice,
	lastQty = <<"40">>,
	bidPrice,
	bidQty = <<"100.0">>,
	askPrice,
	askQty = <<"100.0">>,
	openPrice,
	highPrice,
	lowPrice,
	volume = <<"20000.0">>,
	quoteVolume = <<"10000.0">>,
	openTime = 150000000,
	closeTime = 150000000,
	firstId = 10000000,
	lastId = 15000000,
	count = 10000
}).


%% TYPES


%% DEFINITIONS
-define(INTERVAL, 320).
-define(TABLE, 'ETHBTC_1h').



%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


reset() ->
	gen_server:call(?MODULE, {reset}).


get_exchange_info() ->
	{ok, File} = file:read_file(<<"exchanges/exchange_info.json">>),
	{ok, jiffy:decode(File)}.


%%	1499040000000,      // Open time
%%	"0.01634790",       // Open
%%	"0.80000000",       // High
%%	"0.01575800",       // Low
%%	"0.01577100",       // Close
%%	"148976.11427815",  // Volume
%%	1499644799999,      // Close time
%%	"2434.19055334",    // Quote asset volume
%%	308,                // Number of trades
%%	"1756.87402397",    // Taker buy base asset volume
%%	"28.46694368",      // Taker buy quote asset volume
%%	"17928899.62484339" // Ignore.
get_candles(#{symbol := Symbol, interval := Interval}) ->

	{ok, Ts} = exsim_timer:get_time(),
	TsTunced = trunc_ts(Ts, to_interval(Interval)),

	Data1 = ets:select(binary_to_atom(<<Symbol/binary, "_", Interval/binary>>, latin1), ets:fun2ms(
		fun(X = #candle{timestamp = Timestamp})
			when Timestamp =< TsTunced andalso Timestamp >= TsTunced - 499 * 900000 -> X
		end
	)),

	Data2 = [[Ts1, O, H, L, C, V, Ts2, Q, N, Tb, Tq, I]
		|| {_, Ts1, O, H, L, C, V, Ts2, Q, N, Tb, Tq, I} <- Data1],
	{ok, Data2}.


get_tick(Opts) ->
	gen_server:call(?MODULE, {get_tick}).


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	?INFO("EXSIM: Initiating application...", []),
	erlang:send_after(500, self(), {init}),
	{ok, #state{}}.


handle_call({reset}, _From, State) ->
	{reply, ok, State#state{
		tick = undefined,
		tick_counter = 0,
		candle = undefined
	}};

handle_call({get_tick}, _From, State = #state{tick = TickRecord}) ->
	#tick{
		symbol = Sym,
		priceChange = PC,
		priceChangePercent = PCP,
		weightedAvgPrice = AvgP,
		prevClosePrice = C,
		lastPrice = P,
		lastQty = Q,
		bidPrice = B,
		bidQty = BQ,
		askPrice = A,
		askQty = AQ,
		openPrice = O,
		highPrice = H,
		lowPrice = L,
		volume = V,
		quoteVolume = QV,
		openTime = OTs,
		closeTime = CTs,
		firstId = Id1,
		lastId = Id2,
		count = Count
	} = TickRecord,
	Tick = #{
		symbol => Sym,
		priceChange => PC,
		priceChangePercent => PCP,
		weightedAvgPrice => AvgP,
		prevClosePrice => C,
		lastPrice => P,
		lastQty => Q,
		bidPrice => B,
		bidQty => BQ,
		askPrice => A,
		askQty => AQ,
		openPrice => O,
		highPrice => H,
		lowPrice => L,
		volume => V,
		quoteVolume => QV,
		openTime => OTs,
		closeTime => CTs,
		firstId => Id1,
		lastId => Id2,
		count => Count
	},
	{reply, {ok, Tick}, State}.


handle_cast(_Request, State) ->
	{noreply, State}.


handle_info({init}, State) ->
	% initate candles table
	populate_data_tables(),
	erlang:send_after(?INTERVAL, self(), {interval}),
	{noreply, State#state{interval = '1h'}};

handle_info({interval}, State = #state{tick_counter = TickCounter0,
	candle = Candle0, tick = Tick0, interval = Interval}) ->
	{ok, Ts} = exsim_timer:get_time(),
	Candle1 = get_candle(Ts, Interval),
	{Tick, TickCounter1} = case TickCounter0 of
							   T when T == 0 ->
								   {generate_tick(Candle1, 1), 1};
							   T when T < 12 ->
								   {generate_tick(Candle1, T), T};
							   T when T == 12 andalso Candle1 =/= Candle0 ->
								   {generate_tick(Candle1, 1), 1};
							   _ ->
								   {Tick0, 12}
						   end,


	erlang:send_after(?INTERVAL, self(), {interval}),
	{noreply, State#state{
		candle = Candle1,
		tick_counter = TickCounter1,
		tick = Tick
	}};

handle_info({stop}, State) ->
	{stop, shutdown, State};

handle_info({'EXIT', _, _}, State) ->
	{stop, shutdown, State};

handle_info(M, State) ->
	?WARN("EXSIM: Receiving unexpected msg: ~p", [M]),
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%%%%%%%
%%%    PRIVATE     %%%
%%%%%%%%%%%%%%%%%%%%%%


populate_data_tables() ->
	{ok, Data1} = utils:read_csv(<<"Data/", "ETHBTC-1h.csv">>),
	ets:new(?TABLE, [public, named_table, {keypos, 2}]),
	Data2 = [#candle{
		timestamp = list_to_integer(Ts1),
		open = list_to_binary(O),
		high = list_to_binary(H),
		low = list_to_binary(L),
		close = list_to_binary(C),
		volume = list_to_binary(V),
		close_timestamp = list_to_integer(Ts2),
		quote_volume = list_to_binary(Q),
		number = list_to_integer(N),
		taker_base = list_to_binary(Tb),
		taker_quote = list_to_binary(Tq),
		ignore = list_to_binary(I)
	} || [Ts1, O, H, L, C, V, Ts2, Q, N, Tb, Tq, I] <- Data1],
	true = ets:insert(?TABLE, Data2),
	?INFO("EXSIM: Table ~p is populated with trading data", [?TABLE]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

get_candle(Ts, Size) ->
	CandleTs = trunc_ts(Ts, Size),
	[H|_] = ets:lookup(?TABLE, CandleTs),
	H.


%% Based on https://www.metatrader5.com/en/terminal/help/algotrading/tick_generation
generate_tick(Candle, TickCounter) ->
	#candle{
		open = Open,
		high = High,
		low = Low,
		close = Close,
		timestamp = OpenTs,
		close_timestamp = CloseTs
	} = Candle,
	O = binary_to_float(Open),
	H = binary_to_float(High),
	L = binary_to_float(Low),
	C = binary_to_float(Close),
	OHLC = {O, H, L, C},
	Price = case O of
						   X when X > C ->
							   generate_bear_tick_price(OHLC, TickCounter);
						   X when X < C ->
							   generate_bull_tick_price(OHLC, TickCounter);
						   X when X == C andalso H == L ->
							   generate_point_tick_price(OHLC, TickCounter);
						   X when X == C ->
							   generate_bull_tick_price(OHLC, TickCounter)
					   end,
	Bid = Price,
	Ask = Price + (H - L)/3,
	#tick{
		weightedAvgPrice = list_to_binary(io_lib:format("~.8f",[abs(Ask - Bid)/2 + Bid])),
		lastPrice = list_to_binary(io_lib:format("~.8f",[Price])),
		bidPrice = list_to_binary(io_lib:format("~.8f",[Bid])),
		askPrice = list_to_binary(io_lib:format("~.8f",[Ask])),
		openPrice = Open,
		highPrice = High,
		lowPrice = Low,
		openTime = OpenTs,
		closeTime = CloseTs
	}.


generate_bear_tick_price({O, _H, _L, _C}, 1) ->
	O;
generate_bear_tick_price({O, H, _L, _C}, 2) ->
	O + 0.75 * (H - O);
generate_bear_tick_price({O, H, _L, _C}, 3) ->
	O + 0.5 * (H - O);
generate_bear_tick_price({_O, H, _L, _C}, 4) ->
	H;
generate_bear_tick_price({_O, H, L, _C}, 5) ->
	H - (H - L)/3;
generate_bear_tick_price({O, _H, _L, _C}, 6) ->
	O;
generate_bear_tick_price({_O, H, L, _C}, 7) ->
	H - 2 * (H - L)/3;
generate_bear_tick_price({_O, _H, _L, C}, 8) ->
	C;
generate_bear_tick_price({_O, _H, L, _C}, 9) ->
	L;
generate_bear_tick_price({_O, _H, L, C}, 10) ->
	L + 0.75 * (C - L);
generate_bear_tick_price({_O, _H, L, C}, 11) ->
	L + 0.5 * (C - L);
generate_bear_tick_price({_O, _H, _L, C}, 12) ->
	C.


generate_bull_tick_price({O, _H, _L, _C}, 1) ->
	O;
generate_bull_tick_price({O, _H, L, _C}, 2) ->
	O - 0.75 * (O - L);
generate_bull_tick_price({O, _H, L, _C}, 3) ->
	O - 0.5 * (O - L);
generate_bull_tick_price({_O, _H, L, _C}, 4) ->
	L;
generate_bull_tick_price({_O, H, L, _C}, 5) ->
	(H - L)/3;
generate_bull_tick_price({O, _H, _L, _C}, 6) ->
	O;
generate_bull_tick_price({_O, H, L, _C}, 7) ->
	H - 2 * (H - L)/3;
generate_bull_tick_price({_O, _H, _L, C}, 8) ->
	C;
generate_bull_tick_price({_O, H, _L, _C}, 9) ->
	H;
generate_bull_tick_price({_O, H, _L, C}, 10) ->
	C + 0.25 * (H - C);
generate_bull_tick_price({_O, H, _L, C}, 11) ->
	C + 0.5 * (H - C);
generate_bull_tick_price({_O, _H, _L, C}, 12) ->
	C.


generate_point_tick_price({O, _H, _L, _C}, _) ->
	O.


trunc_ts(Ts, Size) ->
	1000 * (Ts - (Ts rem to_sec(Size))). % convert to ms


to_interval(<<"1m">>) -> '1m';
to_interval(<<"5m">>) -> '5m';
to_interval(<<"15m">>) -> '15m';
to_interval(<<"1h">>) -> '1h'.


to_symbol(<<"ETHBTC">>) -> ethbtc.


to_sec('1m') -> 60;
to_sec('5m') -> 300;
to_sec('15m') -> 900;
to_sec('1h') -> 3600.