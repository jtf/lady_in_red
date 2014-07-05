
The lady in red
===============


#### Description

What can she do for you?  This module provides an erlang interface for
ANSI escape sequences.  Foreground and background colour and text
formatting attributes are supported.  There is a wrapper module called
jo, which can replace all your io:format/n commands with additional
features.

#### Examples

Direct ANSI colour code:

```1> jo:format("The ~31mLady~m in red.").```

By colour names:

```2> jo:format("The ~*mLady~m in red.", [red]).```

True colour is also supported:

```3> jo:format("The ~*tmLady~m in red.", [{rgb, 220,0,0}]).```

Some text formatting:

```4> jo:format("Text can be ~..*mbold~m or ~..*munderlined~m.", [bold, underline] ).```

#### Quick start

```~$ git clone https://github.com/jtf/lady_in_red```

```~$ cd lady_in_red```

Make sure you have rebar in your path.  If not download it from:

https://github.com/rebar/rebar/wiki/rebar

Then build it

```~/lady_in_red$ make```

and test it

```~/lady_in_red$ make test```


#### Further documentation

For more details consult the manual page for jo(3erl).

#### License

The code usage lies under the EPL (Erlang Public License).
See [LICENSE](https://github.com/jtf/lady_in_red/blob/master/LICENSE).
