-module(exsim_app).

-behaviour(application).

-include("lager.hrl").

-export([start/0, start/2, stop/1]).

-define(PORT, 2252).

%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

start() ->
	_ = [application:start(Dep) || Dep <- resolve_deps(exsim),
		not is_otp_base_app(Dep)],
	ok.


start(_Type, _Args) ->

	RouteSpecs = [
		{"/binance/[:resource/[:service]]", exsim_api, []},
		{"/services/[:service]", exsim_api, []},
		{"/[...]", cowboy_static, {priv_dir, exsim, "/"}}
	],
	Dispatch = cowboy_router:compile([
		{'_', RouteSpecs}
	]),
	cowboy:start_clear(http_tradex, [
		{port, ?PORT},
		{num_acceptors,  20}
	],
		#{env => #{dispatch => Dispatch}}
	),
	?INFO("COWBOY: Started on port ~p", [?PORT]),

	Path = get_arg(p),
	exsim_sup:start_link(Path).


stop(_State) ->
	ok.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

dep_apps(App) ->
	application:load(App),
	{ok, Apps} = application:get_key(App, applications),
	Apps.


all_deps(App, Deps) ->
	[[all_deps(Dep, [App | Deps]) || Dep <- dep_apps(App),
		not lists:member(Dep, Deps)], App].


resolve_deps(App) ->
	DepList = all_deps(App, []),
	{AppOrder, _} = lists:foldl(fun(A, {List, Set}) ->
		case sets:is_element(A, Set) of
			true ->
				{List, Set};
			false ->
				{List ++ [A], sets:add_element(A, Set)}
		end
								end,
		{[], sets:new()},
		lists:flatten(DepList)),
	AppOrder.


is_otp_base_app(kernel) ->
	true;
is_otp_base_app(stdlib) ->
	true;
is_otp_base_app(_) ->
	false.


get_arg(Name) ->
	case init:get_argument(Name) of
		{ok,[[L]]} -> L;
		_ -> undefined
	end.
