%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson <dgud@erlang.org>
%%% @copyright (C) 2017, Dan Gudmundsson
%%% @doc
%%%
%%% @end
%%% Created : 24 Aug 2017 by Dan Gudmundsson <dgud@erlang.org>
%%%-------------------------------------------------------------------
-module(ansi).

%% Graphic commands (text properties)
-export([graphic/1, reset/1, string/2]).


-export([clear/0, clear_line/1, top/0, move/2,
         up/1, down/1, forward/1, backward/1]).

%% info
-export([all_colors/0]).

%% Some useful Unicode characters
-export([dot/0]).

dot() -> [9679].

string(Str, Opts) ->
    {Pre,Post} = graphic(Opts),
    lists:flatten([Pre,Str|Post]).

graphic(Opts) ->
    {codes_1(Opts), reset(all)}.

clear() -> "\e[2J".

top() -> "\e[2H".

up(Rs) -> ["\e[", integer_to_list(Rs), $A].
down(Rs) -> ["\e[", integer_to_list(Rs), $B].

forward(Cs) -> ["\e[", integer_to_list(Cs), $C].
backward(Cs) -> ["\e[", integer_to_list(Cs), $D].

move(Row,Col) -> "\e[" ++ integer_to_list(Row) ++ ";" ++ integer_to_list(Col) ++ "H".

clear_line(to_end) -> "\e[0K";
clear_line(to_beg) -> "\e[1K";
clear_line(all) -> "\e[2K".

%% graphic reset
reset(all) -> "\e[0m";
reset(Opts) ->
    Codes = lists:map(fun reset_code/1, Opts),
    Code  = lists:join(";", lists:sort(Codes)),
    [$\e, $[, Code, $m].

codes_1(Opts) ->
    Codes = lists:map(fun code/1, Opts),
    Code  = lists:join(";", lists:sort(Codes)),
    [$\e, $[, Code, $m].

code(Code) ->
    integer_to_list(code_1(Code)).

code_1({fg,Color}) ->
    30 + color(Color);
code_1({bg, Color}) ->
    40 + color(Color);
code_1(bold) ->
    1;
code_1(dim) ->
    2;
code_1(ul) ->
    4;
code_1(blink) ->  %% Does not work in most terminals
    5;
code_1(reverse) ->
    7;
code_1(hidden) ->
    8.

reset_code({fg, _}) -> 39;
reset_code(fg) -> 39;
reset_code({bg, _}) -> 49;
reset_code(bg) -> 49;
reset_code(Code) -> 20 + code(Code).

color(black) -> 0;
color(red) -> 1;
color(green) -> 2;
color(yellow) -> 3;
color(blue) -> 4;
color(magenta) -> 5;
color(cyan) -> 6;
color(l_gray) -> 7;
color(d_gray) -> 60;
color(l_red) -> 61;
color(l_green) -> 62;
color(l_yellow) -> 63;
color(l_blue) -> 64;
color(l_magenta) -> 65;
color(l_cyan) -> 66;
color(white) -> 67.

all_colors() ->
    [black, red, green, yellow, blue, magenta, cyan, l_gray,
     d_gray, l_red, l_green, l_yellow, l_blue, l_magenta, l_cyan, white].
