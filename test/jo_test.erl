-module(jo_test).
-include_lib("eunit/include/eunit.hrl").


jo_test() ->
    ?assertEqual(ok, jo:format("")),
    ?assertEqual(ok, jo:format("", [])),
    ?assertEqual(ok, jo:format(group_leader(), "", [])),
    ?assertEqual(ok, jo:fwrite("")),
    ?assertEqual(ok, jo:fwrite("", [])),
    ?assertEqual(ok, jo:fwrite(group_leader(), "", [])),
    ?assertEqual(ok, jo:reset()).
