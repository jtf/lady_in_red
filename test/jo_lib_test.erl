-module(jo_lib_test).
-include_lib("eunit/include/eunit.hrl").

%%
%% io_lib produces a [] for every ~i found in string; BUG in io_lib?
%%   "a~i" —> [97,[]]    "a~i~ia" —> [97,[],[],97]
%%
la([], Num_of_empty_Strings) ->
    lists:duplicate(Num_of_empty_Strings, []);
la(String, Num_of_empty_Strings) ->
    lists:append(String, lists:duplicate(Num_of_empty_Strings,[])).


m_test()->
    % reset
    ?assertEqual("\e[0m", jo_lib:fwrite("~m", [])),
    ?assertEqual("\e[0m", jo_lib:format("~m", [])),
    ?assertEqual("\e[0m", jo_lib:format("~.m", [])),
    ?assertEqual(la("\e[m",3), jo_lib:format("~*.*.*m", [none,none,none])),

    %% one option
    % foreground
    ?assertEqual("\e[32m",       jo_lib:format("~32m",  [])),
    ?assertEqual(la("\e[32m",1), jo_lib:format("~*m",   [32])),
    ?assertEqual(la("\e[32m",1), jo_lib:format("~*m",   [2])),
    ?assertEqual(la("\e[32m",1), jo_lib:format("~*m",   [green])),
    % background
    ?assertEqual("\e[42m",       jo_lib:format("~.2m",  [])),
    ?assertEqual(la("\e[42m",1), jo_lib:format("~.*m",  [green])),
    % formatting
    ?assertEqual("\e[4m",        jo_lib:format("~..4m", [])),
    ?assertEqual(la("\e[4m",1),  jo_lib:format("~..*m", [underline])),
    ?assertEqual(la("\e[5;4m",1),  jo_lib:format("~..*m", [[blink,underline]])),
    ?assertEqual(la("\e[5;4m",1),  jo_lib:format("~..*m", [[5,4]])),

    %% two options
    ?assertEqual("\e[32;44m",       jo_lib:format("~32.44m", [])),
    ?assertEqual("\e[32;44m",       jo_lib:format("~2.4m",   [])),
    ?assertEqual(la("\e[32;44m",2), jo_lib:format("~*.*m",   [32,44])),
    ?assertEqual(la("\e[32;44m",2), jo_lib:format("~*.*m",   [2,4])),
    ?assertEqual(la("\e[32;44m",2), jo_lib:format("~*.*m",   [green,blue])),
    ?assertEqual("\e[44;4m",        jo_lib:format("~.44.4m", [])),
    ?assertEqual("\e[44;4m",        jo_lib:format("~.4.4m",  [])),
    ?assertEqual(la("\e[44;4m",2),  jo_lib:format("~.*.*m",  [44,4])),
    ?assertEqual(la("\e[44;4m",2),  jo_lib:format("~.*.*m",  [4,4])),
    ?assertEqual(la("\e[44;4m",2),  jo_lib:format("~.*.*m",  [blue, underline])),

    %% three options
    ?assertEqual("\e[32;44;4m",       jo_lib:format("~32.44.4m", [])),
    ?assertEqual(la("\e[32;44;4m",3), jo_lib:format("~*.*.*m",   [32,44,4])),
    ?assertEqual(la("\e[32;44;4m",3), jo_lib:format("~*.*.*m",   [green, blue, underline])),
    ?assertEqual(la("\e[32;44;4m",3), jo_lib:format("~*.*.*m",   [green, blue, [underline]])),
    ?assertEqual(la("\e[32;44;4;1m",3), jo_lib:format("~*.*.*m", [green, blue, [underline,bold]])).


m_colour_names_test() ->
    FGlist = [ {reset, 0}, {black,30}, {red,31}, {green,32}, {yellow,33},
               {blue,34}, {magenta,35}, {cyan,36}, {white,37}, {default_fg, 39} ],
    FGLlist = [ {light_black, 90}, {light_red,     91},
                {light_green, 92}, {light_yellow,  93},
                {light_blue,  94}, {light_magenta, 95},
                {light_cyan,  96}, {light_white,   97} ],

    BGlist = [ {reset, 0},  {black,40}, {red,41}, {green,42}, {yellow,43},
               {blue,44}, {magenta,45}, {cyan,46}, {white,47}, {default_bg, 49} ],
    BGLlist = [ {light_black, 100}, {light_red,     101},
                {light_green, 102}, {light_yellow,  103},
                {light_blue,  104}, {light_magenta, 105},
                {light_cyan,  106}, {light_white,   107} ],

    %% FG
    %% names
    [ ?assertEqual(la("\e["++integer_to_list(B)++"m",1), jo_lib:format("~*m",[A]))
      || {A,B}<-FGlist ],
    [ ?assertEqual(la("\e["++integer_to_list(B)++"m",1), jo_lib:format("~*m",[A]))
      || {A,B}<-FGLlist ],
    % short numbers
    [ ?assertEqual(la("\e["++integer_to_list(30+B)++"m",1), jo_lib:format("~*m",[B]))
      || B<-lists:seq(0,7)++[9] ],
    % numbers
    [ ?assertEqual(la("\e["++integer_to_list(B)++"m",1), jo_lib:format("~*m",[B]))
      || B<-lists:seq(30,37)++[39] ],
    [ ?assertEqual(la("\e["++integer_to_list(B)++"m",1), jo_lib:format("~*m",[B]))
      || B<-lists:seq(90,97)],

    %% BG
    %% names
    [ ?assertEqual(la("\e["++integer_to_list(B)++"m",1), jo_lib:format("~.*m",[A]))
      || {A,B}<-BGlist ],
    [ ?assertEqual(la("\e["++integer_to_list(B)++"m",1), jo_lib:format("~.*m",[A]))
      || {A,B}<-BGLlist ],
    % short numbers
    [ ?assertEqual(la("\e["++integer_to_list(40+B)++"m",1), jo_lib:format("~.*m",[B]))
      || B<-lists:seq(0,7)++[9] ],
    % numbers
    [ ?assertEqual(la("\e["++integer_to_list(B)++"m",1), jo_lib:format("~.*m",[B]))
      || B<-lists:seq(40,47)++[49] ],
    [ ?assertEqual(la("\e["++integer_to_list(B)++"m",1), jo_lib:format("~.*m",[B]))
      || B<-lists:seq(100,107)].


m_attr_names_test() ->
    ATlist = [ {normal, 0}, {bold, 1}, {faint, 2},
               {italic, 3}, {underline, 4}, {blink, 5},
               {blinkfast, 6}, {inverse, 7},
               {invisible, 8}, {hidden, 8},
               {crossed_out, 9}, {doubly_underlined, 21},
               {not_bold_nor_faint, 22}, {not_italic, 23},
               {not_underlined, 24},
               {not_blinking, 25}, {steady, 25},
               {positive, 27}, {visible, 28},
               {not_crossed_out, 29} ],

    %% attributes
    %% names
    [ ?assertEqual(la("\e["++integer_to_list(B)++"m",1), jo_lib:format("~..*m",[A]))
      || {A,B}<-ATlist ],
    %% numbers
    [ ?assertEqual(la("\e["++integer_to_list(B)++"m",1), jo_lib:format("~..*m",[B]))
      || B<-lists:seq(0,9)++lists:seq(21,25)++lists:seq(27,29) ].


m_rgb8_colour_test() ->
    % foreground
    [?assertEqual(la("\e[38;5;" ++ integer_to_list(36*R+6*G+1*B+16)++"m",1),
                  jo_lib:format("~*tm", [{rgb8, R,G,B}]))
     || R <- lists:seq(0,5), G <- lists:seq(0,5), B <- lists:seq(0,5) ],
    % background
    [?assertEqual(la("\e[48;5;" ++ integer_to_list(36*R+6*G+1*B+16)++"m",1),
                  jo_lib:format("~.*tm", [{rgb8, R,G,B}]))
     || R <- lists:seq(0,5), G <- lists:seq(0,5), B <- lists:seq(0,5) ],

    % direct foreground
    [?assertEqual(la("\e[38;5;" ++ integer_to_list(C)++"m",1),
                  jo_lib:format("~*tm", [{rgb8, C}])) || C <- lists:seq(0,255)],

    % colour conversion
    ?assertEqual(la("\e[38;5;226m\e[48;5;51m",2),
      jo_lib:format("~*.*tm", [{rgb8, {rgb,255,220,0}}, {rgb8, {rgb, 0,220,255}}])).

m_web_colour_test() ->
    % foreground
    [?assertEqual(la("\e[38;5;" ++ integer_to_list(C+16)++"m",1),
                  jo_lib:format("~*tm", [{web, C}])) || C <- lists:seq(0,215)].

m_grey_palette_test() ->
    % foreground
    [?assertEqual(la("\e[38;5;" ++ integer_to_list(C+232)++"m",1),
                  jo_lib:format("~*tm", [{grey, C}])) || C <- lists:seq(0,23)],
     ?assertEqual(la("\e[38;5;231m",1), jo_lib:format("~*tm", [{grey, 24}])),
     ?assertEqual(la("\e[38;5;231m",1), jo_lib:format("~*tm", [{gray, 24}])).


m_true_colour_test() ->
    %% colour parsing
    % foreground
    ?assertEqual(la("\e[38;2;255;220;0m",1), jo_lib:format("~*tm", ["#ffdc00"])),
    ?assertEqual(la("\e[38;2;255;221;0m",1), jo_lib:format("~*tm", ["#fd0"])),
    ?assertEqual(la("\e[38;2;255;220;0m",1), jo_lib:format("~*tm", [{rgb,255,220,0}])),
    % background
    ?assertEqual(la("\e[48;2;0;35;255m",1), jo_lib:format("~.*tm", ["#0023ff"])),
    ?assertEqual(la("\e[48;2;0;34;255m",1), jo_lib:format("~.*tm", ["#02f"])),
    ?assertEqual(la("\e[48;2;0;35;255m",1), jo_lib:format("~.*tm", [{rgb,0,35,255}])),
    % formatting
    ?assertEqual(la("\e[4m",1), jo_lib:format("~..*tm", [underline])),
    ?assertEqual(la("\e[4;5m",1), jo_lib:format("~..*tm", [[underline,blink]])),
    % all
    ?assertEqual(la("\e[38;2;255;220;0m\e[48;2;40;40;120m\e[4m",3),
		 jo_lib:format("~*.*.*tm", ["#ffdc00",{rgb,40,40,120},underline])),
    ?assertEqual(la("\e[38;2;255;220;0m\e[48;2;40;40;120m\e[5;4m",3),
		 jo_lib:format("~*.*.*tm", ["#ffdc00",{rgb,40,40,120},[blink,underline]])),
    % as direct string
    ?assertEqual(la("\e[38;2;255;220;0m",1), jo_lib:format("~*tm",["2;255;220;0"])).

% to long .. timeout
%m_all_true_colour_test() ->
%    [?assertEqual(la("\e[38;2;"++integer_to_list(R)++";"
%			       ++integer_to_list(G)++";"
%			       ++integer_to_list(B)++"m",1), jo_lib:format("~*tm", [{rgb,R,G,B}]))
%	|| R<-lists:seq(0,255), G<-lists:seq(0,255), B<-lists:seq(0,255)].


contitional_n_test() ->
    %% the additional brackets come from io_lib
    ?assertEqual(la(["\n"],1),     jo_lib:format("~tn", [true])),
    ?assertEqual(la(["\n\n\n"],1), jo_lib:format("~3tn", [true])),
    ?assertEqual(la([],1),         jo_lib:format("~3tn", [false])),
    ?assertThrow(not_a_boolean_argument, jo_lib:format("~tn",[yes])).



% standard io_lib format tests
pass_through_simple_test() ->
    % empty string
    ?assertEqual(io_lib:format("", []),      jo_lib:format("", [])),

    % normal string
    ?assertEqual(io_lib:format("YAY", []),   jo_lib:format("YAY", [])),

    % without arguments
    ?assertEqual(io_lib:format("~~", []),    jo_lib:format("~~", [])),

    % with one argument
    ?assertEqual(io_lib:format("~i", [nil]), jo_lib:format("~i", [nil])),

    % new line(s)
    ?assertEqual(io_lib:format("~n",  []),   jo_lib:format("~n",  [])),
    ?assertEqual(io_lib:format("~4n", []),   jo_lib:format("~4n", [])),
    ?assertEqual(io_lib:format("~*n", [4]),  jo_lib:format("~*n", [4])).


% all forms of integer
pass_through_integer_test() ->
    %% integer B
    % zero options
    ?assertEqual(io_lib:format("~B",    [4711]),     jo_lib:format("~B",    [4711])),
    % one option
    ?assertEqual(io_lib:format("~10B",  [4711]),     jo_lib:format("~10B",  [4711])),
    ?assertEqual(io_lib:format("~.12B", [4711]),     jo_lib:format("~.12B", [4711])),
    ?assertEqual(io_lib:format("~.. B", [4711]),     jo_lib:format("~.. B", [4711])),
    ?assertEqual(io_lib:format("~*B",   [10,4711]),  jo_lib:format("~*B",   [10,4711])),
    ?assertEqual(io_lib:format("~.*B",  [12,4711]),  jo_lib:format("~.*B",  [12,4711])),
    ?assertEqual(io_lib:format("~..*B", [$a,4711]),  jo_lib:format("~..*B", [$a,4711])),
%    ?assertEqual(io_lib:format("~..*B", [$…,4711]),  jo_lib:format("~..*B", [$…,4711])),
    % two options
    ?assertEqual(io_lib:format("~10.16B", [4711]),    jo_lib:format("~10.16B", [4711])),
    ?assertEqual(io_lib:format("~10.._B", [4711]),    jo_lib:format("~10.._B", [4711])),
    ?assertEqual(io_lib:format("~.12. B", [4711]),    jo_lib:format("~.12. B", [4711])),
    ?assertEqual(io_lib:format("~*.16B",  [10,4711]), jo_lib:format("~*.16B",  [10,4711])),
    ?assertEqual(io_lib:format("~*.._B",  [10,4711]), jo_lib:format("~*.._B",  [10,4711])),
    ?assertEqual(io_lib:format("~.12.*B", [$/,4711]), jo_lib:format("~.12.*B", [$/,4711])),
    % three options
    ?assertEqual(io_lib:format("~5.12. B", [4711]),  jo_lib:format("~5.12. B", [4711])),
    ?assertEqual(io_lib:format("~*.*.*B", [5,12,$_,4711]),jo_lib:format("~*.*.*B", [5,12,$_,4711])),

    %% integer b
    % zero options
    ?assertEqual(io_lib:format("~b",    [4711]),     jo_lib:format("~b",    [4711])),
    % one option
    ?assertEqual(io_lib:format("~10b",  [4711]),     jo_lib:format("~10b",  [4711])),
    ?assertEqual(io_lib:format("~.12b", [4711]),     jo_lib:format("~.12b", [4711])),
    ?assertEqual(io_lib:format("~.. b", [4711]),     jo_lib:format("~.. b", [4711])),
    ?assertEqual(io_lib:format("~*b",   [10,4711]),  jo_lib:format("~*b",   [10,4711])),
    ?assertEqual(io_lib:format("~.*b",  [12,4711]),  jo_lib:format("~.*b",  [12,4711])),
    ?assertEqual(io_lib:format("~..*b", [$…,4711]),  jo_lib:format("~..*b", [$…,4711])),
    % two options
    ?assertEqual(io_lib:format("~10.16b", [4711]),   jo_lib:format("~10.16b", [4711])),
    ?assertEqual(io_lib:format("~10.._b", [4711]),   jo_lib:format("~10.._b", [4711])),
    ?assertEqual(io_lib:format("~.12. b", [4711]),   jo_lib:format("~.12. b", [4711])),
    ?assertEqual(io_lib:format("~*.16b",  [10,4711]), jo_lib:format("~*.16b",  [10,4711])),
    ?assertEqual(io_lib:format("~*.._b",  [10,4711]), jo_lib:format("~*.._b",  [10,4711])),
    ?assertEqual(io_lib:format("~.12.*b", [$/,4711]), jo_lib:format("~.12.*b", [$/,4711])),
    % three options
    ?assertEqual(io_lib:format("~5.12. b", [4711]),  jo_lib:format("~5.12. b", [4711])),
    ?assertEqual(io_lib:format("~*.*.*b", [5,12,$_,4711]),jo_lib:format("~*.*.*b", [5,12,$_,4711])),

    %% integer #
    % zero options
    ?assertEqual(io_lib:format("~#",    [4711]),     jo_lib:format("~#",    [4711])),
    % one option
    ?assertEqual(io_lib:format("~-10#", [4711]),     jo_lib:format("~-10#", [4711])),
    ?assertEqual(io_lib:format("~.12#", [4711]),     jo_lib:format("~.12#", [4711])),
    ?assertEqual(io_lib:format("~.. #", [4711]),     jo_lib:format("~.. #", [4711])),
    ?assertEqual(io_lib:format("~*#",   [10,4711]),  jo_lib:format("~*#",   [10,4711])),
    ?assertEqual(io_lib:format("~.*#",  [12,4711]),  jo_lib:format("~.*#",  [12,4711])),
    ?assertEqual(io_lib:format("~..*#", [$…,4711]),  jo_lib:format("~..*#", [$…,4711])),
    % two options
    ?assertEqual(io_lib:format("~10.16#", [4711]),    jo_lib:format("~10.16#", [4711])),
    ?assertEqual(io_lib:format("~10.._#", [4711]),    jo_lib:format("~10.._#", [4711])),
    ?assertEqual(io_lib:format("~.12. #", [4711]),    jo_lib:format("~.12. #", [4711])),
    ?assertEqual(io_lib:format("~*.16#",  [10,4711]), jo_lib:format("~*.16#",  [10,4711])),
    ?assertEqual(io_lib:format("~*.._#",  [10,4711]), jo_lib:format("~*.._#",  [10,4711])),
    ?assertEqual(io_lib:format("~.9.*#",  [$/,4711]), jo_lib:format("~.9.*#",  [$/,4711])),
    % three options
    ?assertEqual(io_lib:format("~7.12. #", [4711]),         jo_lib:format("~7.12. #", [4711])),
    ?assertEqual(io_lib:format("~*.*.*#",  [7,12,$_,4711]), jo_lib:format("~*.*.*#",  [7,12,$_,4711])),

    %% integer +
    % zero options
    ?assertEqual(io_lib:format("~+",    [4711]),     jo_lib:format("~+",    [4711])),
    % one option
    ?assertEqual(io_lib:format("~10+",  [4711]),     jo_lib:format("~10+",  [4711])),
    ?assertEqual(io_lib:format("~.12+", [4711]),     jo_lib:format("~.12+", [4711])),
    ?assertEqual(io_lib:format("~.. +", [4711]),     jo_lib:format("~.. +", [4711])),
    ?assertEqual(io_lib:format("~*+",   [10,4711]),  jo_lib:format("~*+",   [10,4711])),
    ?assertEqual(io_lib:format("~.*+",  [12,4711]),  jo_lib:format("~.*+",  [12,4711])),
    ?assertEqual(io_lib:format("~..*+", [$…,4711]),  jo_lib:format("~..*+", [$…,4711])),
    % two options
    ?assertEqual(io_lib:format("~10.16+", [4711]),    jo_lib:format("~10.16+", [4711])),
    ?assertEqual(io_lib:format("~10.._+", [4711]),    jo_lib:format("~10.._+", [4711])),
    ?assertEqual(io_lib:format("~.12. +", [4711]),    jo_lib:format("~.12. +", [4711])),
    ?assertEqual(io_lib:format("~*.16+",  [10,4711]), jo_lib:format("~*.16+",  [10,4711])),
    ?assertEqual(io_lib:format("~*.._+",  [10,4711]), jo_lib:format("~*.._+",  [10,4711])),
    ?assertEqual(io_lib:format("~.9.*+",  [$/,4711]), jo_lib:format("~.9.*+",  [$/,4711])),
    % three options
    ?assertEqual(io_lib:format("~7.12. +", [4711]),         jo_lib:format("~7.12. +", [4711])),
    ?assertEqual(io_lib:format("~*.*.*+",  [7,12,$_,4711]), jo_lib:format("~*.*.*+",  [7,12,$_,4711])),

    %% integer x
    % zero options
    ?assertEqual(io_lib:format("~x",    [4711,">>"]),     jo_lib:format("~x", [4711,">>"])),
    % one option
    ?assertEqual(io_lib:format("~10x",  [4711,">>"]),     jo_lib:format("~10x",  [4711,">>"])),
    ?assertEqual(io_lib:format("~*x",   [10,4711,">>"]),  jo_lib:format("~*x",   [10,4711,">>"])),
    ?assertEqual(io_lib:format("~.12x", [4711,">>"]),     jo_lib:format("~.12x", [4711,">>"])),
    ?assertEqual(io_lib:format("~.*x",  [12,4711,">>"]),  jo_lib:format("~.*x",  [12,4711,">>"])),
    ?assertEqual(io_lib:format("~.. x", [4711,">>"]),     jo_lib:format("~.. x", [4711,">>"])),
    ?assertEqual(io_lib:format("~..*x", [$…,4711,">>"]),  jo_lib:format("~..*x", [$…,4711,">>"])),
    % two options
    ?assertEqual(io_lib:format("~10.16x", [4711,">>"]),     jo_lib:format("~10.16x", [4711,">>"])),
    ?assertEqual(io_lib:format("~10.._x", [4711,">>"]),     jo_lib:format("~10.._x", [4711,">>"])),
    ?assertEqual(io_lib:format("~.12. x", [4711,">>"]),     jo_lib:format("~.12. x", [4711,">>"])),
    ?assertEqual(io_lib:format("~*.16x",  [10,4711,">"]),   jo_lib:format("~*.16x",  [10,4711,">"])),
    ?assertEqual(io_lib:format("~*.._x",  [10,4711,">"]),   jo_lib:format("~*.._x",  [10,4711,">"])),
    ?assertEqual(io_lib:format("~.*.*x",  [9,$/,4711,">"]), jo_lib:format("~.*.*x",  [9,$/,4711,">"])),
    % three options
    ?assertEqual(io_lib:format("~5.12. x", [4711,">>"]),         jo_lib:format("~5.12. x", [4711,">>"])),
    ?assertEqual(io_lib:format("~*.*.*x",  [5,12,$_,4711,">>"]), jo_lib:format("~*.*.*x",  [5,12,$_,4711,">>"])),

    %% integer X
    % zero options
    ?assertEqual(io_lib:format("~X",    [4711,">>"]),     jo_lib:format("~X", [4711,">>"])),
    % one option
    ?assertEqual(io_lib:format("~10X",  [4711,">>"]),     jo_lib:format("~10X",  [4711,">>"])),
    ?assertEqual(io_lib:format("~*X",   [10,4711,">>"]),  jo_lib:format("~*X",   [10,4711,">>"])),
    ?assertEqual(io_lib:format("~.12X", [4711,">>"]),     jo_lib:format("~.12X", [4711,">>"])),
    ?assertEqual(io_lib:format("~.*X",  [12,4711,">>"]),  jo_lib:format("~.*X",  [12,4711,">>"])),
    ?assertEqual(io_lib:format("~.. X", [4711,">>"]),     jo_lib:format("~.. X", [4711,">>"])),
    ?assertEqual(io_lib:format("~..*X", [$…,4711,">>"]),  jo_lib:format("~..*X", [$…,4711,">>"])),
    % two options
    ?assertEqual(io_lib:format("~10.16X", [4711,">>"]),     jo_lib:format("~10.16X", [4711,">>"])),
    ?assertEqual(io_lib:format("~10.._X", [4711,">>"]),     jo_lib:format("~10.._X", [4711,">>"])),
    ?assertEqual(io_lib:format("~.12. X", [4711,">>"]),     jo_lib:format("~.12. X", [4711,">>"])),
    ?assertEqual(io_lib:format("~*.16X",  [10,4711,">"]),   jo_lib:format("~*.16X",  [10,4711,">"])),
    ?assertEqual(io_lib:format("~*.._X",  [10,4711,">"]),   jo_lib:format("~*.._X",  [10,4711,">"])),
    ?assertEqual(io_lib:format("~.*.*X",  [9,$/,4711,">"]), jo_lib:format("~.*.*X",  [9,$/,4711,">"])),
    % three options
    ?assertEqual(io_lib:format("~5.12. X", [4711,">>"]),          jo_lib:format("~5.12. X", [4711,">>"])),
    ?assertEqual(io_lib:format("~*.*.*X",  [10,12,$_,4711,">>"]), jo_lib:format("~*.*.*X",  [10,12,$_,4711,">>"])).


% all forms of float
pass_through_float_test() ->
    %% float f
    % zero options
    ?assertEqual(io_lib:format("~f",    [1.23]),     jo_lib:format("~f",   [1.23])),
    % one option
    ?assertEqual(io_lib:format("~10f",  [1.23]),    jo_lib:format("~10f",  [1.23])),
    ?assertEqual(io_lib:format("~.3f",  [1.23]),    jo_lib:format("~.3f",  [1.23])),
    ?assertEqual(io_lib:format("~.. f", [1.23]),    jo_lib:format("~.. f", [1.23])),
    ?assertEqual(io_lib:format("~*f",   [10,1.23]), jo_lib:format("~*f",   [10,1.23])),
    ?assertEqual(io_lib:format("~.*f",  [3,1.23]),  jo_lib:format("~.*f",  [3,1.23])),
    ?assertEqual(io_lib:format("~..*f", [$a,1.23]), jo_lib:format("~..*f", [$a,1.23])),
%    ?assertEqual(io_lib:format("~..*f", [$…,1.23]), jo_lib:format("~..*f", [$…,1.23])),
    % two options
    ?assertEqual(io_lib:format("~10.16f", [1.23]),       jo_lib:format("~10.16f", [1.23])),
    ?assertEqual(io_lib:format("~10.._f", [1.23]),       jo_lib:format("~10.._f", [1.23])),
    ?assertEqual(io_lib:format("~.3. f",  [1.23]),       jo_lib:format("~.3. f",  [1.23])),
    ?assertEqual(io_lib:format("~*.*f",   [10,3,1.23]),  jo_lib:format("~*.*f",   [10,3,1.23])),
    ?assertEqual(io_lib:format("~*..*f",  [10,$_,1.23]), jo_lib:format("~*..*f",  [10,$_,1.23])),
    ?assertEqual(io_lib:format("~.*.*f",  [3,$/,1.23]),  jo_lib:format("~.*.*f",  [3,$/,1.23])),
    % three options
    ?assertEqual(io_lib:format("~10.3. f", [1.23]),         jo_lib:format("~10.3. f", [1.23])),
    ?assertEqual(io_lib:format("~*.*.*f",  [10,3,$_,1.23]), jo_lib:format("~*.*.*f",  [10,3,$_,1.23])),

    %% float e
    % zero options
    ?assertEqual(io_lib:format("~e",    [1.23]),     jo_lib:format("~e",   [1.23])),
    % one option
    ?assertEqual(io_lib:format("~10e",  [1.23]),    jo_lib:format("~10e",  [1.23])),
    ?assertEqual(io_lib:format("~.3e",  [1.23]),    jo_lib:format("~.3e",  [1.23])),
    ?assertEqual(io_lib:format("~.. e", [1.23]),    jo_lib:format("~.. e", [1.23])),
    ?assertEqual(io_lib:format("~*e",   [10,1.23]), jo_lib:format("~*e",   [10,1.23])),
    ?assertEqual(io_lib:format("~.*e",  [3,1.23]),  jo_lib:format("~.*e",  [3,1.23])),
    ?assertEqual(io_lib:format("~..*e", [$…,1.23]), jo_lib:format("~..*e", [$…,1.23])),
    % two options
    ?assertEqual(io_lib:format("~10.16e", [1.23]),       jo_lib:format("~10.16e", [1.23])),
    ?assertEqual(io_lib:format("~10.._e", [1.23]),       jo_lib:format("~10.._e", [1.23])),
    ?assertEqual(io_lib:format("~.3. e",  [1.23]),       jo_lib:format("~.3. e",  [1.23])),
    ?assertEqual(io_lib:format("~*.*e",   [10,3,1.23]),  jo_lib:format("~*.*e",   [10,3,1.23])),
    ?assertEqual(io_lib:format("~*..*e",  [10,$_,1.23]), jo_lib:format("~*..*e",  [10,$_,1.23])),
    ?assertEqual(io_lib:format("~.*.*e",  [3,$/,1.23]),  jo_lib:format("~.*.*e",  [3,$/,1.23])),
    % three options
    ?assertEqual(io_lib:format("~10.3. e", [1.23]),         jo_lib:format("~10.3. e", [1.23])),
    ?assertEqual(io_lib:format("~*.*.*e",  [10,3,$_,1.23]), jo_lib:format("~*.*.*e",  [10,3,$_,1.23])),

    %% float g
    % zero options
    ?assertEqual(io_lib:format("~g",    [1.23]),     jo_lib:format("~g",   [1.23])),
    % one option
    ?assertEqual(io_lib:format("~10g",  [1.23]),    jo_lib:format("~10g",  [1.23])),
    ?assertEqual(io_lib:format("~.3g",  [1.23]),    jo_lib:format("~.3g",  [1.23])),
    ?assertEqual(io_lib:format("~.. g", [1.23]),    jo_lib:format("~.. g", [1.23])),
    ?assertEqual(io_lib:format("~*g",   [10,1.23]), jo_lib:format("~*g",   [10,1.23])),
    ?assertEqual(io_lib:format("~.*g",  [3,1.23]),  jo_lib:format("~.*g",  [3,1.23])),
    ?assertEqual(io_lib:format("~..*g", [$…,1.23]), jo_lib:format("~..*g", [$…,1.23])),
    % two options
    ?assertEqual(io_lib:format("~10.16g", [1.23]),       jo_lib:format("~10.16g", [1.23])),
    ?assertEqual(io_lib:format("~10.._g", [1.23]),       jo_lib:format("~10.._g", [1.23])),
    ?assertEqual(io_lib:format("~.3. g",  [1.23]),       jo_lib:format("~.3. g",  [1.23])),
    ?assertEqual(io_lib:format("~*.*g",   [10,3,1.23]),  jo_lib:format("~*.*g",   [10,3,1.23])),
    ?assertEqual(io_lib:format("~*..*g",  [10,$_,1.23]), jo_lib:format("~*..*g",  [10,$_,1.23])),
    ?assertEqual(io_lib:format("~.*.*g",  [3,$/,1.23]),  jo_lib:format("~.*.*g",  [3,$/,1.23])),
    % three options
    ?assertEqual(io_lib:format("~10.3. g", [1.23]),         jo_lib:format("~10.3. g", [1.23])),
    ?assertEqual(io_lib:format("~*.*.*g",  [10,3,$_,1.23]), jo_lib:format("~*.*.*g",  [10,3,$_,1.23])).


% all forms of character
pass_through_character_test() ->
    % zero options
    ?assertEqual(io_lib:format("~c",    [$j]),     jo_lib:format("~c",    [$j])),
    % one option
    ?assertEqual(io_lib:format("~5c",   [$j]),    jo_lib:format("~5c",   [$j])),
    ?assertEqual(io_lib:format("~.2c",  [$j]),    jo_lib:format("~.2c",  [$j])),
    ?assertEqual(io_lib:format("~.._c", [$j]),    jo_lib:format("~.._c", [$j])),
    ?assertEqual(io_lib:format("~*c",   [5,$j]),  jo_lib:format("~*c",   [5,$j])),
    ?assertEqual(io_lib:format("~.*c",  [2,$j]),  jo_lib:format("~.*c",  [2,$j])),
    ?assertEqual(io_lib:format("~..*c", [$a,$j]), jo_lib:format("~..*c", [$a,$j])),
%    ?assertEqual(io_lib:format("~..*c", [$…,$j]), jo_lib:format("~..*c", [$…,$j])),
    % two options
    ?assertEqual(io_lib:format("~5.2c",  [$j]),       jo_lib:format("~5.2c",  [$j])),
    ?assertEqual(io_lib:format("~5.._c", [$j]),       jo_lib:format("~5.._c", [$j])),
    ?assertEqual(io_lib:format("~.2. c", [$j]),       jo_lib:format("~.2. c", [$j])),
    ?assertEqual(io_lib:format("~*.*c",  [5,2,$j]),   jo_lib:format("~*.*c",  [5,2,$j])),
    ?assertEqual(io_lib:format("~*..*c", [2,$_,$j]),  jo_lib:format("~*..*c", [2,$_,$j])),
    ?assertEqual(io_lib:format("~.*.*c", [5,$/,$j]),  jo_lib:format("~.*.*c", [5,$/,$j])),
    % three options
    ?assertEqual(io_lib:format("~5.2.~c", [$j]),        jo_lib:format("~5.2.~c", [$j])),
    ?assertEqual(io_lib:format("~*.*.*c", [5,2,$_,$j]), jo_lib:format("~*.*.*c", [5,2,$_,$j])).


% all forms of strings and terms
pass_through_terms_test() ->
    Str = [["Hey ", ["Joe! "]], "Where are you going with the gun ..."],
    Str8 = ["Motörhead …"],
    Trm = [{going_downtown, ["to", ["shot", {my_lady}]]}, 4711, 23.5],

    %% s
    % zero options
    ?assertEqual(io_lib:format("~s",      [Str]),     jo_lib:format("~s",      [Str])),
    ?assertEqual(io_lib:format("~ts",     [Str8]),    jo_lib:format("~ts",     [Str8])),
    % one option
    ?assertEqual(io_lib:format("~20s",    [Str]),     jo_lib:format("~20s",    [Str])),
    ?assertEqual(io_lib:format("~.10s",   [Str]),     jo_lib:format("~.10s",   [Str])),
    ?assertEqual(io_lib:format("~.._s",   [Str]),     jo_lib:format("~.._s",   [Str])),
    ?assertEqual(io_lib:format("~*s",     [20,Str]),  jo_lib:format("~*s",     [20,Str])),
    ?assertEqual(io_lib:format("~.*s",    [10,Str]),  jo_lib:format("~.*s",    [10,Str])),
    ?assertEqual(io_lib:format("~..*s",   [$a,Str]),  jo_lib:format("~..*s",   [$a,Str])),
%    ?assertEqual(io_lib:format("~..*s",   [$…,Str]),  jo_lib:format("~..*s",   [$…,Str])),
    % two options
    ?assertEqual(io_lib:format("~20.10s", [Str]),       jo_lib:format("~20.10s", [Str])),
    ?assertEqual(io_lib:format("~20.._s", [Str]),       jo_lib:format("~20.._s", [Str])),
    ?assertEqual(io_lib:format("~.10._s", [Str]),       jo_lib:format("~.10._s", [Str])),
    ?assertEqual(io_lib:format("~*.*s",   [20,10,Str]), jo_lib:format("~*.*s",  [20,10,Str])),
    ?assertEqual(io_lib:format("~*..*s",  [20,$_,Str]), jo_lib:format("~*..*s", [20,$_,Str])),
    ?assertEqual(io_lib:format("~.*.*s",  [10,$_,Str]), jo_lib:format("~.*.*s", [10,$_,Str])),
    % three options
    ?assertEqual(io_lib:format("~20.10._s",  [Str]),           jo_lib:format("~20.10._s",  [Str])),
    ?assertEqual(io_lib:format("~*.*.*s",    [20,10,$_,Str]),  jo_lib:format("~*.*.*s",    [20,10,$_,Str])),
    ?assertEqual(io_lib:format("~20.10._ts", [Str8]),          jo_lib:format("~20.10._ts", [Str8])),
    ?assertEqual(io_lib:format("~*.*.*ts",   [20,10,$_,Str8]), jo_lib:format("~*.*.*ts",   [20,10,$_,Str8])),

    %% w
    % zero options
    ?assertEqual(io_lib:format("~w",      [Trm]),     jo_lib:format("~w",      [Trm])),
    % one option
    ?assertEqual(io_lib:format("~20w",    [Trm]),     jo_lib:format("~20w",    [Trm])),
    ?assertEqual(io_lib:format("~.10w",   [Trm]),     jo_lib:format("~.10w",   [Trm])),
    ?assertEqual(io_lib:format("~.._w",   [Trm]),     jo_lib:format("~.._w",   [Trm])),
    ?assertEqual(io_lib:format("~*w",     [20,Trm]),  jo_lib:format("~*w",     [20,Trm])),
    ?assertEqual(io_lib:format("~.*w",    [10,Trm]),  jo_lib:format("~.*w",    [10,Trm])),
    ?assertEqual(io_lib:format("~..*w",   [$…,Trm]),  jo_lib:format("~..*w",   [$…,Trm])),
    % two options
    ?assertEqual(io_lib:format("~20.10w", [Trm]),       jo_lib:format("~20.10w", [Trm])),
    ?assertEqual(io_lib:format("~20.._w", [Trm]),       jo_lib:format("~20.._w", [Trm])),
    ?assertEqual(io_lib:format("~.10._w", [Trm]),       jo_lib:format("~.10._w", [Trm])),
    ?assertEqual(io_lib:format("~*.*w",   [20,10,Trm]), jo_lib:format("~*.*w",  [20,10,Trm])),
    ?assertEqual(io_lib:format("~*..*w",  [20,$_,Trm]), jo_lib:format("~*..*w", [20,$_,Trm])),
    ?assertEqual(io_lib:format("~.*.*w",  [10,$_,Trm]), jo_lib:format("~.*.*w", [10,$_,Trm])),
    % three options
    ?assertEqual(io_lib:format("~20.10._w", [Trm]),          jo_lib:format("~20.10._w", [Trm])),
    ?assertEqual(io_lib:format("~*.*.*w",   [20,10,$_,Trm]), jo_lib:format("~*.*.*w", [20,10,$_,Trm])),

    %% p
    % zero options
    ?assertEqual(io_lib:format("~p",      [Trm]),     jo_lib:format("~p",      [Trm])),
    ?assertEqual(io_lib:format("~lp",     [Trm]),     jo_lib:format("~lp",     [Trm])),
    % one option
    ?assertEqual(io_lib:format("~20p",    [Trm]),     jo_lib:format("~20p",    [Trm])),
    ?assertEqual(io_lib:format("~.10p",   [Trm]),     jo_lib:format("~.10p",   [Trm])),
    ?assertEqual(io_lib:format("~.._p",   [Trm]),     jo_lib:format("~.._p",   [Trm])),
    ?assertEqual(io_lib:format("~*p",     [20,Trm]),  jo_lib:format("~*p",     [20,Trm])),
    ?assertEqual(io_lib:format("~.*p",    [10,Trm]),  jo_lib:format("~.*p",    [10,Trm])),
    ?assertEqual(io_lib:format("~..*p",   [$…,Trm]),  jo_lib:format("~..*p",   [$…,Trm])),
    % two options
    ?assertEqual(io_lib:format("~20.10p", [Trm]),       jo_lib:format("~20.10p", [Trm])),
    ?assertEqual(io_lib:format("~20.._p", [Trm]),       jo_lib:format("~20.._p", [Trm])),
    ?assertEqual(io_lib:format("~.10._p", [Trm]),       jo_lib:format("~.10._p", [Trm])),
    ?assertEqual(io_lib:format("~*.*p",   [20,10,Trm]), jo_lib:format("~*.*p",  [20,10,Trm])),
    ?assertEqual(io_lib:format("~*..*p",  [20,$_,Trm]), jo_lib:format("~*..*p", [20,$_,Trm])),
    ?assertEqual(io_lib:format("~.*.*p",  [10,$_,Trm]), jo_lib:format("~.*.*p", [10,$_,Trm])),
    % three options
    ?assertEqual(io_lib:format("~20.10._p", [Trm]),          jo_lib:format("~20.10._p", [Trm])),
    ?assertEqual(io_lib:format("~*.*.*p",   [20,10,$_,Trm]), jo_lib:format("~*.*.*p", [20,10,$_,Trm])),

    %% W
    % zero options
    ?assertEqual(io_lib:format("~W",      [Trm,2]),     jo_lib:format("~W",      [Trm,2])),
    % one option
    ?assertEqual(io_lib:format("~20W",    [Trm,2]),     jo_lib:format("~20W",    [Trm,2])),
    ?assertEqual(io_lib:format("~.10W",   [Trm,2]),     jo_lib:format("~.10W",   [Trm,2])),
    ?assertEqual(io_lib:format("~.._W",   [Trm,2]),     jo_lib:format("~.._W",   [Trm,2])),
    ?assertEqual(io_lib:format("~*W",     [20,Trm,2]),  jo_lib:format("~*W",     [20,Trm,2])),
    ?assertEqual(io_lib:format("~.*W",    [10,Trm,2]),  jo_lib:format("~.*W",    [10,Trm,2])),
    ?assertEqual(io_lib:format("~..*W",   [$…,Trm,2]),  jo_lib:format("~..*W",   [$…,Trm,2])),
    % two options
    ?assertEqual(io_lib:format("~20.10W", [Trm,2]),       jo_lib:format("~20.10W", [Trm,2])),
    ?assertEqual(io_lib:format("~20.._W", [Trm,2]),       jo_lib:format("~20.._W", [Trm,2])),
    ?assertEqual(io_lib:format("~.10._W", [Trm,2]),       jo_lib:format("~.10._W", [Trm,2])),
    ?assertEqual(io_lib:format("~*.*W",   [20,10,Trm,2]), jo_lib:format("~*.*W",  [20,10,Trm,2])),
    ?assertEqual(io_lib:format("~*..*W",  [20,$_,Trm,2]), jo_lib:format("~*..*W", [20,$_,Trm,2])),
    ?assertEqual(io_lib:format("~.*.*W",  [10,$_,Trm,2]), jo_lib:format("~.*.*W", [10,$_,Trm,2])),
    % three options
    ?assertEqual(io_lib:format("~20.10._W", [Trm,2]),          jo_lib:format("~20.10._W", [Trm,2])),
    ?assertEqual(io_lib:format("~*.*.*W",   [20,10,$_,Trm,2]), jo_lib:format("~*.*.*W", [20,10,$_,Trm,2])),

    % P
    % zero options
    ?assertEqual(io_lib:format("~P",      [Trm,2]),     jo_lib:format("~P",      [Trm,2])),
    % one option
    ?assertEqual(io_lib:format("~20P",    [Trm,2]),     jo_lib:format("~20P",    [Trm,2])),
    ?assertEqual(io_lib:format("~.10P",   [Trm,2]),     jo_lib:format("~.10P",   [Trm,2])),
    ?assertEqual(io_lib:format("~.._P",   [Trm,2]),     jo_lib:format("~.._P",   [Trm,2])),
    ?assertEqual(io_lib:format("~*P",     [20,Trm,2]),  jo_lib:format("~*P",     [20,Trm,2])),
    ?assertEqual(io_lib:format("~.*P",    [10,Trm,2]),  jo_lib:format("~.*P",    [10,Trm,2])),
    ?assertEqual(io_lib:format("~..*P",   [$…,Trm,2]),  jo_lib:format("~..*P",   [$…,Trm,2])),
    % two options
    ?assertEqual(io_lib:format("~20.10P", [Trm,2]),       jo_lib:format("~20.10P", [Trm,2])),
    ?assertEqual(io_lib:format("~20.._P", [Trm,2]),       jo_lib:format("~20.._P", [Trm,2])),
    ?assertEqual(io_lib:format("~.10._P", [Trm,2]),       jo_lib:format("~.10._P", [Trm,2])),
    ?assertEqual(io_lib:format("~*.*P",   [20,10,Trm,2]), jo_lib:format("~*.*P",  [20,10,Trm,2])),
    ?assertEqual(io_lib:format("~*..*P",  [20,$_,Trm,2]), jo_lib:format("~*..*P", [20,$_,Trm,2])),
    ?assertEqual(io_lib:format("~.*.*P",  [10,$_,Trm,2]), jo_lib:format("~.*.*P", [10,$_,Trm,2])),
    % three options
    ?assertEqual(io_lib:format("~20.10._P", [Trm,2]),          jo_lib:format("~20.10._P", [Trm,2])),
    ?assertEqual(io_lib:format("~*.*.*P",   [20,10,$_,Trm,2]), jo_lib:format("~*.*.*P", [20,10,$_,Trm,2])).

