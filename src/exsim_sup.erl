-module(exsim_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

-spec start_link(tradex:key_path()) ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Path) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Path]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

init([Path]) ->
	ExsimServer = #{
		id => exsim,
		start => {exsim, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [exsim]
	},
	ExsimTimer = #{
		id => exsim_timer,
		start => {exsim_timer, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [exsim_timer]
	},
	Children = [ExsimServer, ExsimTimer],
	RestartStrategy = {one_for_one, 10, 300},
	{ok, {RestartStrategy, Children}}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
