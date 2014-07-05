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
-module(jo).
-author('Jonas Melzer <jtf@fsfe.org>').

-export([ fwrite/1, fwrite/2, fwrite/3,
	  format/1, format/2, format/3,
	  reset/0]).

%%%
%%% Wrap it
%%%
fwrite(Fmt) ->
    format(Fmt, []).
fwrite(Fmt, Args) ->
    format(default_output(), Fmt, Args).
fwrite(IO, Fmt, Args) ->
    format(IO, Fmt, Args).

format(Fmt) ->
    format(Fmt, []).
format(Fmt, Args) ->
    format(default_output(), Fmt, Args).
format(IO, Fmt, Args) ->
    io:format(IO, jo_lib_fmt:process_format_string(Fmt, Args), Args).

reset() ->
    io:format("\e[0m").


%%%
%%% Internal functions
%%%

default_output() -> group_leader().
