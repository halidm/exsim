-module(exsim_timer).

-behaviour(gen_server).

-include("global.hrl").
-include("lager.hrl").
-include("print.hrl").


-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([get_time/0, set_time/1]).

-ifdef(TEST).
-compile(export_all).
-endif.


-record(state, {
	time_scale,
	ts_provisional,
	ts_actual,
	time_now,
	hour_now
}).


%% TYPES


%% DEFINITIONS
-define(INTERVAL, 500).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get_time() ->
	gen_server:call(?MODULE, {get_time}).


set_time(Timestamp) ->
	gen_server:call(?MODULE, {set_time, Timestamp}).


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	TimeScale = ?TIME_SCALE,
	TimeStart = ?TIME_START,
	TsActual = get_ts(),
	?INFO("EXSIM: Timer started at ~p", [datetime_to_iso(timestamp_to_datetime(TsActual))]),
	?INFO("EXSIM: Provisional Start Time: ~p - ~p (scale: ~px)",
		[datetime_to_iso(TimeStart), datetime_to_timestamp(TimeStart), TimeScale]),
	erlang:send_after(?INTERVAL, self(), {interval}),
	{ok, #state{
		time_scale = TimeScale,
		ts_provisional = datetime_to_timestamp(TimeStart),
		ts_actual = TsActual
	}}.


handle_call({get_time}, _From, State = #state{time_now = Time}) ->
	{reply, {ok, Time}, State};

handle_call({set_time, Timestamp}, _From, State) ->
	exsim:reset(),
	{reply, ok, State#state{
		ts_provisional = Timestamp,
		ts_actual = get_ts()
	}}.


handle_cast(_Request, State) ->
	{noreply, State}.


handle_info({interval},
	State = #state{
		time_scale = TimeScale,
		ts_provisional = TsProvisional,
		ts_actual = TsActual,
		hour_now = Hour0
	}) ->
	Now = get_ts(),
	Ts = TimeScale * (Now - TsActual) + TsProvisional,

	Hour1 = get_ts_hour(Ts),

	case Hour1 =/= Hour0 of
		true when (Hour1 band 1) == 0 ->
			?print("  $ec$ec",
				[
					<<"[*] Server Time: ">>,
					list_to_binary(datetime_to_iso(timestamp_to_datetime(Ts)))
				]);
		_ -> ok
	end,

	erlang:send_after(?INTERVAL, self(), {interval}),
	{noreply, State#state{time_now = Ts, hour_now = Hour1}};

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


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

get_ts() ->
	get_ts(now).

get_ts(now) ->
	datetime_to_timestamp(calendar:now_to_universal_time(erlang:timestamp()));
get_ts(Time) ->
	datetime_to_timestamp(Time).


get_ts_hour(Ts) ->
	{{_, _, _}, {Hours, _, _}} = timestamp_to_datetime(Ts),
	Hours.


datetime_to_timestamp({{Year, Month, Day}, {Hours, Minutes, Seconds}}) ->
	(calendar:datetime_to_gregorian_seconds(
		{{Year, Month, Day}, {Hours, Minutes, Seconds}}
	) - 62167219200).


timestamp_to_datetime(Timestamp) ->
	calendar:gregorian_seconds_to_datetime(trunc(Timestamp) + 62167219200).


datetime_to_iso({{Year, Month, Day}, {Hours, Minutes, Seconds}}) ->
	lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
		[Year, Month, Day, Hours, Minutes, Seconds])).
