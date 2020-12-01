-module(exsim_api).

-include("global.hrl").
-include("lager.hrl").

-export([
	init/2,
	allowed_methods/2,
	content_types_provided/2,
	content_types_accepted/2,
	options/2
]).

-export([handle_get/2, handle_post/2]).


init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.


allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.


content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, handle_get}
	], Req, State}.


content_types_accepted(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, '*'}, handle_post}
	], Req, State}.


options(Req, State) ->
	Headers = #{
		<<"Access-Control-Allow-Methods">> => <<"POST, GET, OPTIONS, DELETE, PUT">>,
		<<"Access-Control-Allow-Origin">> => <<"*">>,
		<<"Access-Control-Allow-Headers">> => <<"Authorize, Cache-Control, x-requested-with, Content-Type, origin, authorization, accept, client-security-token, x-csrf-token, X-CSRF-Token">>
	},
	{ok, cowboy_req:set_resp_headers(Headers, Req), State}.


handle_get(Req, State) ->

	Request = cowboy_req:binding(resource, Req),

	ParsedQs = cowboy_req:parse_qs(Req),
	Opts = [{binary_to_atom(K, latin1), V} || {K, V} <- ParsedQs],

	?INFO("EXSIM: API CALL => GET /~p  ~p", [binary_to_atom(Request, latin1), Opts]),

	Resp = case {Request} of

			   %% GET /exchangeInfo
			   {<<"exchangeInfo">>} ->
				   case exsim:get_exchange_info() of
					   {ok, Info} ->
						   jiffy:encode(Info);
					   _ ->
						   eval_error(else)
				   end;

			   %% GET /klines?symbol=ETHBTC&interval=5m
			   {<<"klines">>} ->
				   case exsim:get_candles(cowboy_req:match_qs([symbol, interval], Req)) of
					   {ok, Klines} ->
						   jiffy:encode(Klines);
					   _ ->
						   eval_error(else)
				   end;

			   %% GET /ticker/24?symbol=ETHBTC
			   {<<"ticker">>} ->
				   case exsim:get_tick(Opts) of
					   {ok, Tick} ->
						   jiffy:encode(Tick);
					   _ ->
						   eval_error(else)
				   end;


			   {<<"health">>} ->
				   <<"HEALTHY">>
		   end,
	Headers = #{
		<<"Access-Control-Allow-Methods">> => <<"POST, GET, OPTIONS, DELETE, PUT">>,
		<<"Access-Control-Allow-Origin">> => <<"*">>,
		<<"Access-Control-Allow-Headers">> => <<"Authorize, Cache-Control, x-requested-with, Content-Type, origin, authorization, accept, client-security-token, x-csrf-token, X-CSRF-Token">>
	},
	{Resp, cowboy_req:set_resp_headers(Headers, Req), State}.


handle_post(Req, State) ->

	Request = cowboy_req:binding(service, Req),

	{ok, Body, _} = cowboy_req:read_body(Req),
	BodyMap = jiffy:decode(Body, [return_maps]),

	?INFO("EXSIM: API CALL => POST /~p  ~p", [binary_to_atom(Request, latin1), BodyMap]),

	Resp = case {Request} of

			   %% POST /time
			   {<<"time">>} ->
				   #{<<"timestamp">> := Timestamp} = BodyMap,
				   exsim_timer:set_time(Timestamp),
				   true;

			  _ ->
				   false
		   end,
	Headers = #{
		<<"Access-Control-Allow-Methods">> => <<"POST, GET, OPTIONS, DELETE, PUT">>,
		<<"Access-Control-Allow-Origin">> => <<"*">>,
		<<"Access-Control-Allow-Headers">> => <<"Authorize, Cache-Control, x-requested-with, Content-Type, origin, authorization, accept, client-security-token, x-csrf-token, X-CSRF-Token">>
	},
	{Resp, cowboy_req:set_resp_headers(Headers, Req), State}.



validate_service(<<"health">>) -> health;
validate_service(<<"report">>) -> report;
validate_service(_) -> call_error.


eval_error(not_found) ->
	"Resource not available";
eval_error(call_error) ->
	"Call not available";
eval_error(_) ->
	"Unknown error".



