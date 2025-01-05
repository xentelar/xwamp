-module(wamp_message_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("xwamp.hrl").

-compile(export_all).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
all() ->
  common:all().

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
groups() ->
  [{main, [parallel], common:tests(?MODULE)}].

%% =============================================================================
%% JSON
%% =============================================================================

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
abort_test(_) ->
  Uri = <<"com.example.foo">>,
  Details = #{<<"bar">> => <<"baz">>},

  Expected = #abort{reason_uri = Uri, details = Details},

  ?assertEqual(Expected, xwamp_message_builder:abort(Details, Uri)).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
error_test(_) ->
  ErrorUri = <<"com.example.error">>,
  Details = #{<<"bar">> => <<"baz">>},
  Args = [1,2,3,4],
  KWArgs = #{<<"kwargs">> => <<"kwargs">>},

  Expected = #error{request_type = 1,
                    request_id = 1,
                    details = Details,
                    error_uri = ErrorUri,
                    args = undefined,
                    kwargs = undefined
                  },

  ?assertEqual(Expected, xwamp_message_builder:error(1, 1, Details, ErrorUri)),

  Expected0 = #error{request_type = 1,
                      request_id = 1,
                      details = Details,
                      error_uri = ErrorUri,
                      args = Args,
                      kwargs = undefined
                    },

  ?assertEqual(Expected0, xwamp_message_builder:error(1, 1, Details, ErrorUri, Args)),

    Expected1 = #error{request_type = 1,
                      request_id = 1,
                      details = Details,
                      error_uri = ErrorUri,
                      args = Args,
                      kwargs = KWArgs
                    },

  ?assertEqual(Expected1, xwamp_message_builder:error(1, 1, Details, ErrorUri, Args, KWArgs)).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
call_test(_) ->
  Uri = <<"com.example.foo">>,

  Opts0 = #{},

  ?assertEqual(
    #call{
      request_id = 1,
      options = Opts0,
      procedure_uri = Uri,
      args = undefined,
      kwargs = undefined
    },
    xwamp_message_builder:call(1, Opts0, Uri, [])
  ),

  Opts1 = #{<<"ppt_scheme">> => <<"foo">>},
  
  ?assertEqual(
    #call{
      request_id = 1,
      options = Opts1,
      procedure_uri = Uri,
      args = undefined,
      kwargs = undefined
    },
    xwamp_message_builder:call(1, Opts1, Uri, [], #{})
  ),

  ?assertEqual(
    #call{
      request_id = 1,
      options = Opts1,
      procedure_uri = Uri,
      args = undefined,
      kwargs = #{<<"a">> => 100}
    },
    xwamp_message_builder:call(1, Opts1, Uri, [], #{<<"a">> => 100})
  ),

  ?assertEqual(
    #call{
      request_id = 1,
      options = Opts1,
      procedure_uri = Uri,
      args = [1],
      kwargs = undefined
    },
    xwamp_message_builder:call(1, Opts1, Uri, [1])
  ),

  ?assertEqual(
    #call{
      request_id = 1,
      options = Opts1,
      procedure_uri = Uri,
      args = [<<>>],
      kwargs = undefined
    },
    xwamp_message_builder:call(1, Opts1, Uri, [<<>>])
  ),
  
  ?assertEqual(
    #call{
      request_id = 1,
      options = Opts1,
      procedure_uri = Uri,
      args = [1],
      kwargs = #{<<"a">> => 100}
    },
    xwamp_message_builder:call(1, Opts1, Uri, [1], #{<<"a">> => 100})
  ).
