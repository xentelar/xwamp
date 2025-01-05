-module(wamp_uri_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("xwamp.hrl").

-compile(export_all).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
all() ->
  [
    test_strict,
    validate_exact,
    validate_exact_error,
    validate_prefix,
    validate_prefix_error,
    validate_wildcard,
    validate_wildcard_error,
    empty_uri,
    match
  ].

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
init_per_suite(Config) ->
  _ = application:ensure_all_started(xwamp),
	application:set_env(xwamp, uri_strictness, strict, [{persistent, false}]),
  Config.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
end_per_suite(_) ->
  ok.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
test_strict(_) ->
  List = [
    <<"foo.b-a-r">>,
    <<"foo.b a r">>
  ],
  lists:foreach(
    fun(URI) ->
      ?assertEqual(
        false,
        xwamp_uri:is_valid(URI, strict)
      )
    end,
    List
  ).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
validate_exact(_) ->
  List = [
    <<"a">>,
    <<"a.foo">>,
    <<"a.foo.c">>,
    <<"a.foo.c.1">>,
    <<"a.foo.c.1.1">>
  ],
  lists:foreach(
    fun(URI) ->
      ?assertEqual(
        URI,
        xwamp_uri:validate(URI, ?EXACT_MATCH)
      )
    end,
    List
  ).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
validate_exact_error(_) ->
  List = [
    <<>>,
    <<"a.">>,
    <<"a.foo.">>,
    <<"a.foo.c.">>,
    <<".">>,
    <<"..">>,
    <<"...">>,
    <<".foo">>,
    <<"a..">>,
    <<"a.foo..">>,
    <<".foo..">>
  ],
  lists:foreach(
    fun(URI) ->
      ?assertError(
        {invalid_uri, URI},
        xwamp_uri:validate(URI, ?EXACT_MATCH)
      )
    end,
    List
  ).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
validate_prefix(_) ->
  List = [
    <<"a">>,
    <<"a.">>,
    <<"a.foo.">>,
    <<"a.foo.c">>,
    <<"a.foo.c.">>
  ],
  lists:foreach(
    fun(URI) ->
      ?assertEqual(
        URI,
        xwamp_uri:validate(URI, ?PREFIX_MATCH)
      )
    end,
    List
  ).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
validate_prefix_error(_) ->
  List = [
    <<".">>,
    <<"..">>,
    <<"...">>,
    <<".foo">>,
    <<"a..">>,
    <<"a.foo..">>,
    <<".foo..">>
  ],
  lists:foreach(
    fun(URI) ->
      ?assertError(
        {invalid_uri, URI},
        xwamp_uri:validate(URI, ?PREFIX_MATCH)
      )
    end,
    List
  ).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
validate_wildcard(_) ->
  List = [
    <<".">>,
    <<"..">>,
    <<"...">>,
    <<".foo">>,
    <<"a..">>,
    <<"a.foo.">>,
    <<"a.foo..">>,
    <<".foo..">>
  ],
  lists:foreach(
    fun(URI) ->
      ?assertEqual(
        URI,
        xwamp_uri:validate(URI, ?WILDCARD_MATCH)
      )
    end,
    List
  ).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
validate_wildcard_error(_) ->
  List = [
    <<>>
  ],
  lists:foreach(
    fun(URI) ->
      ?assertError(
        {invalid_uri, URI},
        xwamp_uri:validate(URI, ?WILDCARD_MATCH)
      )
    end,
    List
  ).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
empty_uri(_) ->
  application:set_env(xwamp, uri_strictness, strict, [{persistent, false}]),
  ?assertEqual(false, xwamp_uri:is_valid(<<>>)),
  ?assertEqual(false, xwamp_uri:is_valid(<<>>, ?PREFIX_MATCH)),
  ?assertEqual(false, xwamp_uri:is_valid(<<>>, ?EXACT_MATCH)),
  ?assertEqual(false, xwamp_uri:is_valid(<<>>, ?WILDCARD_MATCH)),

  application:set_env(xwamp, uri_strictness, loose, [{persistent, false}]),
  ?assertEqual(false, xwamp_uri:is_valid(<<>>)),
  ?assertEqual(true, xwamp_uri:is_valid(<<>>, ?PREFIX_MATCH)),
  ?assertEqual(false, xwamp_uri:is_valid(<<>>, ?EXACT_MATCH)),
  ?assertEqual(false, xwamp_uri:is_valid(<<>>, ?WILDCARD_MATCH)),

  %% Irrespective of uri_strictness setting
  ?assertEqual(true, xwamp_uri:is_valid(<<>>, loose_prefix)),
  ?assertEqual(false, xwamp_uri:is_valid(<<>>, loose)),
  ?assertEqual(false, xwamp_uri:is_valid(<<>>, strict)),
  ?assertEqual(false, xwamp_uri:is_valid(<<>>, strict_prefix)),
  ?assertEqual(false, xwamp_uri:is_valid(<<>>, loose_allow_empty)),
  ?assertEqual(false, xwamp_uri:is_valid(<<>>, strict_allow_empty)),

  ok.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
match(Config) ->
  application:set_env(xwamp, uri_strictness, strict, [{persistent, false}]),
  do_match(Config),

  application:set_env(xwamp, uri_strictness, loose, [{persistent, false}]),
  do_match(Config).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
do_match(_) ->
  ?assertError(badarg, xwamp_uri:match(<<>>, <<"a">>, ?EXACT_MATCH)),
  ?assertError(badarg, xwamp_uri:match(<<>>, <<"a">>, ?WILDCARD_MATCH)),
  ?assertEqual(false, xwamp_uri:match(<<>>, <<"a">>, ?PREFIX_MATCH)),
  ?assertEqual(true, xwamp_uri:match(<<>>, <<>>, ?PREFIX_MATCH)),
  ?assertEqual(false, xwamp_uri:match(<<"a">>, <<"b">>, ?EXACT_MATCH)),
  ?assert(xwamp_uri:match(<<"a">>, <<"a">>, ?EXACT_MATCH)),
  ?assert(xwamp_uri:match(<<"a">>, <<"a">>, ?PREFIX_MATCH)),
  ?assert(xwamp_uri:match(<<"a.">>, <<"a">>, ?PREFIX_MATCH)),
  ?assert(xwamp_uri:match(<<"a.b">>, <<"a">>, ?PREFIX_MATCH)),

  ?assert(xwamp_uri:match(<<"a">>, <<"a">>, ?WILDCARD_MATCH)),
  ?assert(xwamp_uri:match(<<"a.b">>, <<".">>, ?WILDCARD_MATCH)),
  ?assert(xwamp_uri:match(<<"a.b">>, <<"a.">>, ?WILDCARD_MATCH)),
  ?assert(xwamp_uri:match(<<"a.b">>, <<".b">>, ?WILDCARD_MATCH)),
  ?assert(xwamp_uri:match(<<"a.b">>, <<"a.b">>, ?WILDCARD_MATCH)).