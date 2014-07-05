-module(jo_lib_fmt_test).
-include_lib("eunit/include/eunit.hrl").

% Tests are only for ignore parameter counting and corresponding format string.
% The parameter, colour and attribute tests are done in jo_lib_test.erl.

% ~m
process_format_string_m_test() ->
    % 0 options
    ?assertEqual("\e[0m",             jo_lib_fmt:process_format_string("~m",      [])),
    % 1 option
    ?assertEqual("\e[32m~i",          jo_lib_fmt:process_format_string("~*m",     [32])),
    ?assertEqual("\e[41m~i",          jo_lib_fmt:process_format_string("~.*m",    [41])),
    ?assertEqual("\e[1m~i",           jo_lib_fmt:process_format_string("~..*m",   [1])),
    % 2 options
    ?assertEqual("\e[32;41m~i~i",     jo_lib_fmt:process_format_string("~*.*m",   [32,41])),
    ?assertEqual("\e[32;1m~i~i",      jo_lib_fmt:process_format_string("~*..*m",  [32,1])),
    ?assertEqual("\e[41;1m~i~i",      jo_lib_fmt:process_format_string("~.*.*m",  [41,1])),
    % 3 options
    ?assertEqual("\e[32;41;1m~i~i~i", jo_lib_fmt:process_format_string("~*.*.*m", [32,41,1])).

% ~n
process_format_string_n_test() ->
    ?assertEqual("~n~i", jo_lib_fmt:process_format_string("~tn", [true])),
    ?assertEqual("~i",   jo_lib_fmt:process_format_string("~tn", [false])).
