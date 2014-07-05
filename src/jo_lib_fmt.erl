%%%———————————————————————————————————————————————————————————————————————
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Initial Developer of the Original Code is Jonas Melzer.
%%% Copyright 2014 Jonas Melzer <jtf@fsfe.org>. All Rights Reserved.
%%%———————————————————————————————————————————————————————————————————————

%%%
%%% Just out
%%%
-module(jo_lib_fmt).
-author('Jonas Melzer <jtf@fsfe.org>').

-include_lib("eunit/include/eunit.hrl").


-export([
	 process_format_string/2
	]).


process_format_string(Fmt, Args) ->
    lists:flatten(process_format_string(Fmt, Args, [])).


%%%---------------------------------------------------------------------
%%%  Internal functions
%%%
process_format_string([$~|Fmt], Args, FList) ->
    {RestFmt, RArgs1, Fseq1, FPPMC} = read_fppmc(Fmt, [$~], Args),
    {Fseq2, RArgs2} = gen_seq(Fseq1, FPPMC, RArgs1),
    process_format_string(RestFmt, RArgs2, [Fseq2|FList]);

process_format_string([C|Fmt], Args, Nfmt) ->
    process_format_string(Fmt, Args, [C|Nfmt]);

process_format_string([], _Args,  Nfmt) ->
    lists:reverse(Nfmt).


ignores(ArgNum) ->
    lists:flatten(lists:duplicate(ArgNum, "~i")).


%%%---------------------------------------------------------------------
%%%  ANSI escape sequence generation
%%%

%%%
%%%  use ANSI paramaters \esc[
%%%

%%
%% format with true colour ~*.*.*tm
%%
gen_seq(_, {_S, FG, BG, Pd, $t, $m, ArgNum}, Args) ->
% debug: io:format(" | (o^O) ~p,~p,~p, $t$M ~p| ~n", [FG,BG,Pd,ArgNum]),

    F = case FG of
	    none -> "";
	    _else1 -> "\e[38;" ++ parse_colour(FG) ++ "m"
        end,

    B = case BG of
	    none -> "";
	    _else2 -> "\e[48;" ++ parse_colour(BG) ++ "m"
        end,

    P = case Pd of
	    none -> "";
	    _else3-> "\e[" ++ attr_string(Pd) ++ "m"
	end,

    Fseq =   F ++ B ++ P ++  ignores(ArgNum),

    {Fseq,Args};


%%%
%%%  \esc ##;##;# m for colour and text attributes
%%%
% without parameters defaults to reset  ~m
gen_seq(_, {_S, none, none, none, none, $m, 0     }, Args) -> {"\e[0m", Args};

% with fg or bg or attr set, only if not truecolour  ~*.*.*m
gen_seq(_, {_S,F,P,Pd,Md,$m,ArgNum}, Args) when (Md =/= $t) ->
    Fseq = gen_escm(fg_string(F), bg_string(P), attr_string(Pd))++ignores(ArgNum),
    {Fseq, Args};

%%%
%%%  conditional newline only if Arg is true
%%%
gen_seq(_Fseq, {_S,Width,_,_,$t,$n,_ArgNum}, Args) ->
    % is not done in signature, otherwise default sequence (the next) will match
    [Arg|Tl] = Args,

    W = if
	    Width == none -> "";
	    Width >= 0 -> integer_to_list(Width)
    end,
    if
	Arg == true  -> {"~" ++ W ++ "n~i", Tl};
	Arg == false -> {"~i", Tl};
	true         -> throw(not_a_boolean_argument)
    end;

% default sequence for pass-trough to io:format  ~*.*.*_  (_=accepted by io:format)
gen_seq(Fseq, _FPPMC, Args) ->
    {Fseq, Args}.


%
% TODO: replace this with generic list version \e 1;2;3;4;5;… m
%
gen_escm(none, none, none) -> "\e[m";
gen_escm(F,    none, none) -> gen_escm(F);
gen_escm(none, P,    none) -> gen_escm(P);
gen_escm(none, none, Pd  ) -> gen_escm(Pd);
gen_escm(F,    P,    none) -> gen_escm(F, P);
gen_escm(none, P,    Pd  ) -> gen_escm(P, Pd);
gen_escm(F,    none, Pd  ) -> gen_escm(F, Pd);
gen_escm(F,    P,    Pd  ) -> 
    "\e[" ++ F ++";" ++ P ++ ";" ++ Pd ++ "m".
gen_escm(A1) ->
    "\e[" ++ A1 ++ "m".
gen_escm(A1, A2) ->
    "\e[" ++ A1 ++ ";" ++ A2 ++ "m".


%%%---------------------------------------------------------------------
%%%  colour to string conversion
%%%

%hex
parse_colour([$#, R1, R2, G1, G2, B1, B2]) ->
    "2;" ++
	integer_to_list(list_to_integer([R1,R2], 16)) ++ ";" ++
	integer_to_list(list_to_integer([G1,G2], 16)) ++ ";" ++
	integer_to_list(list_to_integer([B1,B2], 16));

%short hex
parse_colour([$#, R, G, B]) ->
    "2;" ++
	integer_to_list(list_to_integer([R,R], 16)) ++ ";" ++
	integer_to_list(list_to_integer([G,G], 16)) ++ ";" ++
	integer_to_list(list_to_integer([B,B], 16));

%direct string
parse_colour(String) when is_list(String) ->
    String;

%rgb-tuple
parse_colour({rgb, R, G, B}) when (R+G+B >=0), (R=<255), (G=<255), (B=<255) ->
    "2;" ++
	integer_to_list(R) ++ ";" ++
	integer_to_list(G) ++ ";" ++
	integer_to_list(B);

% 216 colour tuple, each channel from 0 to 5
parse_colour({rgb8, R, G, B}) when (R+G+B >=0), (R=<5), (G=<5), (B=<5) ->
    "5;" ++ integer_to_list(36*R+6*G+1*B+16);

% colour conversion from 16 to 8bit
parse_colour({rgb8, {rgb, R,G,B}}) when (R+G+B >=0), (R=<255), (G=<255), (B=<255) ->
    parse_colour({rgb8, R div 43, G div 43, B div 43});

% direct, standard palette (0..31), 216 colour (32..231), grey scale (232..255)
parse_colour({rgb8, C}) when (C>=0), (C=<255) ->
    "5;" ++ integer_to_list(C);

% 216 colour palette (0..215), "web safe" colours 3bit-red 3bit-green 3bit-blue
parse_colour({web, C}) when (C>=0), (C=<215) ->
    "5;" ++ integer_to_list(C+16);

% 25 steps grey scale (0..24) at end of colour table (added full white, 24)
parse_colour({gray, G})	->
    parse_colour({grey, G});
parse_colour({grey, G}) when (G>=0), (G=<23) ->
    "5;" ++ integer_to_list(G+232);
parse_colour({grey, 24}) ->
    "5;231".


%%%---------------------------------------------------------------------
%%%  ANSI colour/attribute value mapping
%%%

% foreground colours
fg_string(none) -> none;
fg_string(I)    -> integer_to_list(fg(I)).

fg(I)  when (I>=0),  (I=<7)  -> 30 + I;
fg(I)  when (I==9);  (I==39) -> 39;
fg(I)  when (I>=30), (I=<37) -> I;
fg(I)  when (I>=90), (I=<97) -> I;
fg(reset)   -> 0;
fg(black)   -> 30;
fg(red)     -> 31;
fg(green)   -> 32;
fg(yellow)  -> 33;
fg(blue)    -> 34;
fg(magenta) -> 35;
fg(cyan)    -> 36;
fg(white)   -> 37;
fg(default_fg)  -> 39;  % same as next
fg(light_black)  -> 90;
fg(light_red)    -> 91;
fg(light_green)  -> 92;
fg(light_yellow) -> 93;
fg(light_blue)   -> 94;
fg(light_magenta)-> 95;
fg(light_cyan)   -> 96;
fg(light_white)  -> 97.


% background colours
bg_string(none) -> none;
bg_string(I)    -> integer_to_list(bg(I)).

bg(I)  when (I>=0),  (I=<7) -> 40 + I;
bg(I)  when (I==9);  (I==49) -> 49;
bg(I)  when (I>=40), (I=<47) -> I;
bg(I)  when (I>=100), (I=<107) -> I;
bg(reset)   -> 0;
bg(black)   -> 40;
bg(red)     -> 41;
bg(green)   -> 42;
bg(yellow)  -> 43;
bg(blue)    -> 44;
bg(magenta) -> 45;
bg(cyan)    -> 46;
bg(white)   -> 47;
bg(default_bg) -> 49;  % same as next
bg(light_black)  -> 100;
bg(light_red)    -> 101;
bg(light_green)  -> 102;
bg(light_yellow) -> 103;
bg(light_blue)   -> 104;
bg(light_magenta)-> 105;
bg(light_cyan)   -> 106;
bg(light_white)  -> 107.


% text formatting
attr_string(none) -> none;

% combined list of attributes
attr_string(L) when is_list(L) ->
    %tl for removing first ";"
    tl(lists:flatten([";"++attr_string(Attr) || Attr<-L ]));
attr_string(I) ->
    integer_to_list(attr(I)).

attr(normal)    -> 0;
attr(bold)      -> 1;
attr(faint)     -> 2;
attr(italic)    -> 3;
attr(underline) -> 4;
attr(blink)     -> 5;
attr(blinkfast) -> 6;
attr(inverse)   -> 7;
attr(invisible) -> 8;  % alias to next
attr(hidden)    -> 8;
attr(crossed_out) -> 9;
attr(I) when is_integer(I), (I>=0), (I=<9) -> I;
attr(doubly_underlined)  -> 21;
attr(not_bold_nor_faint) -> 22;
attr(not_italic)         -> 23;
attr(not_underlined)     -> 24;
attr(not_blinking)       -> 25;  % alias to next
attr(steady)             -> 25;
attr(positive)           -> 27;
attr(visible)            -> 28;
attr(not_crossed_out)    -> 29;
attr(I) when is_integer(I), (I>=21), (I=<25) -> I;
attr(I) when is_integer(I), (I>=27), (I=<29) -> I.


%%%---------------------------------------------------------------------
%%% parse io:format-style control sequence
%%% example: ~[-]field.precision.pad[type]control ~-10.4._f
%%%
read_fppmc(     Fmt,   Fseq,  Args) ->
    {Sign,      RFmt1, Fseq1}          = swallow_char( Fmt,   Fseq,  $-),
    {Field,     RFmt2, Fseq2, RArgs1, IAN1}
		                       = extract_value(RFmt1, Fseq1, Args),
    {_PrecDot,   RFmt3, Fseq3}         = swallow_char( RFmt2, Fseq2, $.),
    {Precision, RFmt4, Fseq4, RArgs2, IAN2}
                                       = extract_value(RFmt3, Fseq3, RArgs1),
    {PadDot,    RFmt5, Fseq5}          = swallow_char( RFmt4, Fseq4, $.),
    {Pad,       RFmt6, Fseq6, RArgs3, IAN3}  =
		if PadDot == none ->  {none, RFmt5, Fseq5, RArgs2,  0};
		   true           ->  extract_pad(     RFmt5, Fseq5, RArgs2)
		end,
    {Unicode,  RFmt7, Fseq7}           = swallow_char( RFmt6, Fseq6, $t),
    {Latin,    RFmt8, Fseq8}           = swallow_char( RFmt7, Fseq7, $l),
    {Control,   RFmt9, Fseq9}          = extract_char( RFmt8, Fseq8),
    Mod = case {Unicode, Latin} of
	      {none, none} -> none;
	      {$t  , none} -> $t;
	      {none, $l}   -> $l
          end,
    RArgs4 = case Control of
	% zero
	C when (C==$~); (C==$n)
	     -> RArgs3;
	% remove one argument
	C when (C==$#); (C==$b); (C==$B); (C==$c); (C==$e); (C==$f); (C==$g);
	       (C==$i); (C==$p); (C==$s); (C==$w); (C==$x)
	     -> tl(RArgs3);
	% remove two arguments
	C when (C==$P); (C==$W); (C==$X)
	    -> tl(tl(RArgs3));
	_Else -> RArgs3
    end,
    { RFmt9, RArgs4, lists:reverse(Fseq9),
      {Sign, Field, Precision, Pad, Mod, Control, IAN1+IAN2+IAN3} }.


% remove leading character iff it is the given character
swallow_char([Char|Fmt], Fseq, Char) ->
    {Char, Fmt, [Char|Fseq] };
swallow_char(Fmt, Fseq, _Char) ->
    {none, Fmt, Fseq}.


% extract leading character
extract_char([C|Fmt], Fseq) ->
    {C, Fmt, [C|Fseq]}.


% extract characters  as long first character is integer
% 'A' can be any type not only Integer
extract_value([$*|Fmt], Fseq, [A|Args]) ->
    {A, Fmt, [$*|Fseq], Args, 1};
extract_value([C|Fmt], Fseq, Args) when is_integer(C), C>=$0, C=< $9 ->
    extract_integer([C|Fmt], Fseq, Args, 0);
extract_value(Fmt, Fseq, Args) ->
    {none, Fmt, Fseq, Args, 0}.


% extract characters as long as first character is integer
extract_integer([C|Fmt], Fseq, Args, Value)
  when is_integer(C), C >= $0, C =< $9 ->
    extract_integer(Fmt, [C|Fseq], Args, 10*Value + (C-$0));
extract_integer(Fmt, Fseq, Args, Value) ->
    {Value, Fmt, Fseq, Args, 0}.


% extract * and first arg from argument list or first character
extract_pad([$*|Fmt], Fseq, [A|Args]) ->
    {A, Fmt, [$*|Fseq], Args, 1};
extract_pad([C|Fmt], Fseq, Args) when is_integer(C), C>=$0, C=< $9 ->
    {C-$0, Fmt, [C|Fseq], Args, 0};
extract_pad([C|Fmt], Fseq, Args) ->
    {{char,C}, Fmt, [C|Fseq], Args, 0}.


%%%---------------------------------------------------------------------
%%%  eunit tests of internal functions
%%%

swallow_char_test() ->
    ?assertEqual({$-, "abc", "-a"},  swallow_char("-abc", "a", $-)),
%    ?assertEqual({$…, "abc", "…a"},  swallow_char("…abc", "a", $…)),
    ?assertEqual({none, "abc", "a"}, swallow_char("abc", "a", $-)).

extract_char_test() ->
    ?assertEqual({$a, "bc", "ad"}, extract_char("abc", "d")),
%    ?assertEqual({$…, "",   "…"},  extract_char("…",   "")),
    ?assertEqual({$a, "",   "a"},  extract_char("a",   "")).

%should fail
%    ?assertEqual({$a, "", "a"}, jo_lib_fmt:extract_char("", "")).

extract_integer_test() ->
    ?assertEqual({0,    "ab", "",      [],  0}, extract_integer("ab",     "",  [],  0)),
    ?assertEqual({0,    "",   "",      [],  0}, extract_integer("",       "",  [],  0)),
    ?assertEqual({1234, "",   "4321",  [],  0}, extract_integer("1234",   "",  [],  0)),
    ?assertEqual({1234, "ab", "4321c", [d], 0}, extract_integer("1234ab", "c", [d], 0)).

