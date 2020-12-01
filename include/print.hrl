
%%% Usage:
%%%
%%% printing with ~p for all terms
%%% ?print("foreground      %k %r %g %y %b %m %c %w", [black, red, green, yellow, blue, magenta, cyan, white]),
%%% ?print("background      %K %R %G %Y %B %M %C %W", [black, red, green, yellow, blue, magenta, cyan, white]),
%%% ?print("effects         %e %d %i %u %l", [emphasis, dim, italic, underline, blink]),
%%% ?print("color effects   %er %dm %ig %uy %lc", [emphasis_red, dim_magenta, italic_green, underline_yellow, blink_cyan]),
%%%
%%% printing with ~ts for strings
%%% ?print("foreground      $k $r $g $y $b $m $c $w", [<<"black">>, <<"red">>, <<"green">>, <<"yellow">>, <<"blue">>, <<"magenta">>, <<"cyan">>, <<"white">>]),
%%% ?print("background      $K $R $G $Y $B $M $C $W", [<<"black">>, <<"red">>, <<"green">>, <<"yellow">>, <<"blue">>, <<"magenta">>, <<"cyan">>, <<"white">>]),
%%% ?print("effects         $e $d $i $u $l", [<<"emphasis">>, <<"dim">>, <<"italic">>, <<"underline">>, <<"blink">>]),
%%% ?print("color effects   $er $dm $ig $uy $lc", [<<"emphasis_red">>, <<"dim_magenta">>, <<"italic_green">>, <<"underline_yellow">>, <<"blink_cyan">>]),

%%% Created : 20. Jan 2017 6:43 AM
%%%-------------------------------------------------------------------


-define(_print_colors, [
	% effects with colors
	{"\\%e([krgybmcwnKRGYBMCWN])", "\e[1m\\%\\1\e[00m"},   % emphasis bold
	{"\\%d([krgybmcwnKRGYBMCWN])", "\e[2m\\%\\1\e[22m"},   % dim
	{"\\%i([krgybmcwnKRGYBMCWN])", "\e[3m\\%\\1\e[23m"},   % italic
	{"\\%u([krgybmcwnKRGYBMCWN])", "\e[4m\\%\\1\e[24m"},   % underline
	{"\\%l([krgybmcwnKRGYBMCWN])", "\e[5m\\%\\1\e[25m"},   % blink
	% effects without colors
	{"\\%e", "\e[1m~p\e[00m"},   % emphasis bold
	{"\\%d", "\e[2m~p\e[22m"},   % dim
	{"\\%i", "\e[3m~p\e[23m"},   % italic
	{"\\%u", "\e[4m~p\e[24m"},   % underline
	{"\\%l", "\e[5m~p\e[25m"},   % blink
	% foreground
	{"\\%k", "\e[30m~p\e[39m"},   % black
	{"\\%r", "\e[31m~p\e[39m"},   % red
	{"\\%g", "\e[32m~p\e[39m"},   % green
	{"\\%y", "\e[33m~p\e[39m"},   % yellow
	{"\\%b", "\e[34m~p\e[39m"},   % blue
	{"\\%m", "\e[35m~p\e[39m"},   % magenta (purple)
	{"\\%c", "\e[36m~p\e[39m"},   % cyan
	{"\\%w", "\e[37m~p\e[39m"},   % cyan
	{"\\%n", "\e[39m~p\e[39m"},    % default
	% background
	{"\\%K", "\e[40m~p\e[49m"},   % black
	{"\\%R", "\e[30m\e[41m~p\e[49m\e[39m"},   % red
	{"\\%G", "\e[30m\e[42m~p\e[49m\e[39m"},   % green
	{"\\%Y", "\e[30m\e[43m~p\e[49m\e[39m"},   % yellow
	{"\\%B", "\e[30m\e[44m~p\e[49m\e[39m"},   % blue
	{"\\%M", "\e[30m\e[45m~p\e[49m\e[39m"},   % magenta (purple)
	{"\\%C", "\e[30m\e[46m~p\e[49m\e[39m"},   % cyan
	{"\\%W", "\e[30m\e[47m~p\e[49m\e[39m"},   % white
	{"\\%N", "\e[30m\e[49m~p\e[49m\e[39m"},    % default
%%
	% effects with colors
	{"\\$e([krgybmcwnKRGYBMCWN])", "\e[1m\\$\\1\e[00m"},   % emphasis bold
	{"\\$d([krgybmcwnKRGYBMCWN])", "\e[2m\\$\\1\e[22m"},   % dim
	{"\\$i([krgybmcwnKRGYBMCWN])", "\e[3m\\$\\1\e[23m"},   % italic
	{"\\$u([krgybmcwnKRGYBMCWN])", "\e[4m\\$\\1\e[24m"},   % underline
	{"\\$l([krgybmcwnKRGYBMCWN])", "\e[5m\\$\\1\e[25m"},   % blink
	% effects without colors
	{"\\$e", "\e[1m~ts\e[00m"},   % emphasis bold
	{"\\$d", "\e[2m~ts\e[22m"},   % dim
	{"\\$i", "\e[3m~ts\e[23m"},   % italic
	{"\\$u", "\e[4m~ts\e[24m"},   % underline
	{"\\$l", "\e[5m~ts\e[25m"},   % blink
	% foreground
	{"\\$k", "\e[30m~ts\e[39m"},   % black
	{"\\$r", "\e[31m~ts\e[39m"},   % red
	{"\\$g", "\e[32m~ts\e[39m"},   % green
	{"\\$y", "\e[33m~ts\e[39m"},   % yellow
	{"\\$b", "\e[34m~ts\e[39m"},   % blue
	{"\\$m", "\e[35m~ts\e[39m"},   % magenta (purple)
	{"\\$c", "\e[36m~ts\e[39m"},   % cyan
	{"\\$w", "\e[37m~ts\e[39m"},   % cyan
	{"\\$n", "\e[39m~ts\e[39m"},    % default
	% background
	{"\\$K", "\e[40m~ts\e[49m"},   % black
	{"\\$R", "\e[30m\e[41m~ts\e[49m\e[39m"},   % red
	{"\\$G", "\e[30m\e[42m~ts\e[49m\e[39m"},   % green
	{"\\$Y", "\e[30m\e[43m~ts\e[49m\e[39m"},   % yellow
	{"\\$B", "\e[30m\e[44m~ts\e[49m\e[39m"},   % blue
	{"\\$M", "\e[30m\e[45m~ts\e[49m\e[39m"},   % magenta (purple)
	{"\\$C", "\e[30m\e[46m~ts\e[49m\e[39m"},   % cyan
	{"\\$W", "\e[30m\e[47m~ts\e[49m\e[39m"},   % white
	{"\\$N", "\e[30m\e[49m~ts\e[49m\e[39m"}   % default
%%	,{"([^,])$","\\1~n"}  % add a ~n at the end by default unless we have a ,
]).
-define(print(Format),
	?__do_print(Format, [self(), ?MODULE, ":", ?FUNCTION_NAME, "/", ?FUNCTION_ARITY, ?LINE])).
-define(print(Format, Args),
	?__do_print(Format, [self(), ?MODULE, ":", ?FUNCTION_NAME, "/", ?FUNCTION_ARITY, ?LINE] ++ Args)).
-define(__do_print(Format, Args),
	io:format(lists:foldl(fun({Pattern, Replacement}, Data) ->
		re:replace(Data, Pattern, Replacement, [{return, list}, global])
						  end,
		% if the formatting string starts with :, print the output on the same line as the function stamp
		case Format of
			[$, | _] -> tl(Format);
			[$: | _] -> "%d %d$d%dg$d%db %dc " ++ tl(Format);
			_ -> "%d %d$d%dg$d%db %dc~n" ++ Format
		end,
		?_print_colors) ++ "~n",
		case Format of
			[$, | _] -> tl(tl(tl(tl(tl(tl(tl(Args))))))); _ -> Args end)).