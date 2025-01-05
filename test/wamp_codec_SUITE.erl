-module(wamp_codec_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

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
hello_json_test(_) ->
  M = xwamp_message_builder:hello(<<"realm1">>, #{
    <<"roles">> => #{
      <<"caller">> => #{}
    }}),
  Bin = xwamp_codec:encode(M, json),

  ?assertEqual(
    hello,
    xwamp_codec:decode_message_name(Bin, json)
  ),
  ?assertMatch(
    [M], xwamp_codec:decode(Bin, json)
  ).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
welcome_json_test(_) ->
  M = xwamp_message_builder:welcome(1, #{
    <<"realm">> => <<"realm1">>,
    <<"authid">> => <<"foo">>,
    <<"authrole">> => <<"default">>,
    <<"roles">> => #{
      <<"dealer">> => #{},
      <<"broker">> => #{}
    }}),
  Bin = xwamp_codec:encode(M, json),
  welcome = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
abort_json_test(_) ->
  M = xwamp_message_builder:abort(#{<<"message">> => <<"foo">>}, <<"wamp.error.foo">>),
  Bin = xwamp_codec:encode(M, json),
  abort = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
challenge_json_test(_) ->
  M = xwamp_message_builder:challenge(<<"foo">>, #{}),
  Bin = xwamp_codec:encode(M, json),
  challenge = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
authenticate_json_test(_) ->
  M = xwamp_message_builder:authenticate(<<"foo">>, #{}),
  Bin = xwamp_codec:encode(M, json),
  authenticate = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
goodbye_json_test(_) ->
  M = xwamp_message_builder:goodbye(
    #{<<"message">> => <<"The host is shutting down now.">>},
    <<"wamp.error.system_shutdown">>
  ),
  Bin = xwamp_codec:encode(M, json),
  ?assertEqual(
    goodbye,
    xwamp_codec:decode_message_name(Bin, json)
  ),
  ?assertMatch(
    [M], xwamp_codec:decode(Bin, json)
  ).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
error_json_test(_) ->
  M = xwamp_message_builder:error(0, 1, #{<<"test0">> => <<"test0">>, 
                                          <<"test1">> => <<"test1">>}, 
                                  <<"wamp.error.foo">>),
  Bin = xwamp_codec:encode(M, json),
  error = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
error_json_2_test(_) ->
  M = xwamp_message_builder:error(0, 1, #{<<"test0">> => <<"test0">>, 
                                          <<"test1">> => <<"test1">>}, 
                                <<"wamp.error.foo">>, [1,2,3,4]),
  Bin = xwamp_codec:encode(M, json),
  error = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
error_json_3_test(_) ->
  M = xwamp_message_builder:error(0, 1, 
                                #{<<"test0">> => <<"test0">>, 
                                  <<"test1">> => <<"test1">>
                                }, 
                              <<"wamp.error.foo">>, [1,2,3,4], 
                              #{<<"kwa0">> => <<"kwa0">>, 
                                <<"kwa1">> => <<"kwa1">>
                              }),
  Bin = xwamp_codec:encode(M, json),
  error = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
publish_json_test(_) ->
  M = xwamp_message_builder:publish(1, #{}, <<"xwamp.test.topic1">>),
  Bin = xwamp_codec:encode(M, json),
  publish = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
publish_json_2_test(_) ->
  M = xwamp_message_builder:publish(1, #{}, <<"xwamp.test.topic1">>),
  Bin = xwamp_codec:encode(M, json),
  publish = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
publish_json_3_test(_) ->
  M = xwamp_message_builder:publish(1, #{}, <<"xwamp.test.topic1">>, [], #{}),
  Bin = xwamp_codec:encode(M, json),
  publish = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
published_json_test(_) ->
  M = xwamp_message_builder:published(1, 2),
  Bin = xwamp_codec:encode(M, json),
  published = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
subscribe_json_test(_) ->
  M = xwamp_message_builder:subscribe(1, #{}, <<"xwamp.test.topic1">>),
  Bin = xwamp_codec:encode(M, json),
  subscribe = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
subscribed_json_test(_) ->
  M = xwamp_message_builder:subscribed(1, 3),
  Bin = xwamp_codec:encode(M, json),
  subscribed = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
unsubscribe_json_test(_) ->
  M = xwamp_message_builder:unsubscribe(1, 3),
  Bin = xwamp_codec:encode(M, json),
  unsubscribe = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
unsubscribed_json_test(_) ->
  M = xwamp_message_builder:unsubscribed(1),
  Bin = xwamp_codec:encode(M, json),
  unsubscribed = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
event_json_test(_) ->
  M = xwamp_message_builder:event(3, 2, #{}),
  Bin = xwamp_codec:encode(M, json),
  event = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
event_json_2_test(_) ->
  M = xwamp_message_builder:event(3, 2, #{}, []),
  Bin = xwamp_codec:encode(M, json),
  event = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
event_json_3_test(_) ->
  M = xwamp_message_builder:event(3, 2, #{}, [], #{}),
  Bin = xwamp_codec:encode(M, json),
  event = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
call_json_test(_) ->
  M = xwamp_message_builder:call(1, #{}, <<"xwamp.test.procedure1">>),
  Bin = xwamp_codec:encode(M, json),
  call = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
call_json_2_test(_) ->
  M = xwamp_message_builder:call(1, #{}, <<"xwamp.test.procedure1">>, []),
  Bin = xwamp_codec:encode(M, json),
  call = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
call_json_3_test(_) ->
  M = xwamp_message_builder:call(1, #{}, <<"xwamp.test.procedure1">>, [], #{}),
  Bin = xwamp_codec:encode(M, json),
  call = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
call_json_4_test(_) ->
  Args = [
    [{bar, pid_to_list(self())}]
  ],
  M0 = xwamp_message_builder:call(1, #{}, <<"foo">>, Args, #{}),
  Bin = xwamp_codec:encode(M0, json),
  call = xwamp_codec:decode_message_name(Bin, json),
  [M1] = xwamp_codec:decode(Bin, json),
  ?assertMatch(
    {call, 1, _, <<"foo">>, [#{<<"bar">> := _}],
    undefined},
    M1
  ).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
cancel_json_test(_) ->
  M = xwamp_message_builder:cancel(1, #{}),
  Bin = xwamp_codec:encode(M, json),
  cancel = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
result_json_test(_) ->
  M = xwamp_message_builder:result(1, #{}),
  Bin = xwamp_codec:encode(M, json),
  result = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
result_json_2_test(_) ->
  M = xwamp_message_builder:result(1, #{}, []),
  Bin = xwamp_codec:encode(M, json),
  result = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
result_json_3_test(_) ->
  M = xwamp_message_builder:result(1, #{}, [], #{}),
  Bin = xwamp_codec:encode(M, json),
  result = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
register_json_test(_) ->
  M = xwamp_message_builder:register(1, #{}, <<"xwamp.test.procedure1">>),
  Bin = xwamp_codec:encode(M, json),
  register = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
registered_json_2_test(_) ->
  M = xwamp_message_builder:registered(1, 4),
  Bin = xwamp_codec:encode(M, json),
  registered = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
unregister_json_3_test(_) ->
  M = xwamp_message_builder:unregister(1, 4),
  Bin = xwamp_codec:encode(M, json),
  unregister = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
unregistered_json_test(_) ->
  M = xwamp_message_builder:unregistered(1),
  Bin = xwamp_codec:encode(M, json),
  unregistered = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
invocation_json_test(_) ->
  M = xwamp_message_builder:invocation(1, 4, #{}),
  Bin = xwamp_codec:encode(M, json),
  invocation = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
invocation_json_2_test(_) ->
  M = xwamp_message_builder:invocation(1, 4, #{}, []),
  Bin = xwamp_codec:encode(M, json),
  invocation = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
invocation_json_3_test(_) ->
  M = xwamp_message_builder:invocation(1, 4, #{}, [], #{}),
  Bin = xwamp_codec:encode(M, json),
  invocation = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
interrupt_json_test(_) ->
  M = xwamp_message_builder:interrupt(1, #{}),
  Bin = xwamp_codec:encode(M, json),
  interrupt = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
yield_json_test(_) ->
  M = xwamp_message_builder:yield(1, #{}),
  Bin = xwamp_codec:encode(M, json),
  yield = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
yield_json_2_test(_) ->
  M = xwamp_message_builder:yield(1, #{}, []),
  Bin = xwamp_codec:encode(M, json),
  yield = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
yield_json_3_test(_) ->
  M = xwamp_message_builder:yield(1, #{}, [], #{}),
  Bin = xwamp_codec:encode(M, json),
  yield = xwamp_codec:decode_message_name(Bin, json),
  [M] = xwamp_codec:decode(Bin, json).

%% =============================================================================
%% MSGPACK
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
hello_msgpack_test(_) ->
  M = xwamp_message_builder:hello(<<"realm1">>, #{
    <<"roles">> => #{
      <<"caller">> => #{}
    }}),
  Bin = xwamp_codec:encode(M, msgpack),
  hello = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
welcome_msgpack_test(_) ->
  M = xwamp_message_builder:welcome(1, #{
    <<"realm">> => <<"realm1">>,
    <<"authid">> => <<"foo">>,
    <<"authrole">> => <<"default">>,
    <<"roles">> => #{
      <<"dealer">> => #{},
      <<"broker">> => #{}
    }}),
  Bin = xwamp_codec:encode(M, msgpack),
  welcome = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
abort_msgpack_test(_) ->
  M = xwamp_message_builder:abort(#{<<"message">> => <<"foo">>}, <<"wamp.error.foo">>),
  Bin = xwamp_codec:encode(M, msgpack),
  abort = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
challenge_msgpack_test(_) ->
  M = xwamp_message_builder:challenge(<<"foo">>, #{}),
  Bin = xwamp_codec:encode(M, msgpack),
  challenge = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
authenticate_msgpack_test(_) ->
  M = xwamp_message_builder:authenticate(<<"foo">>, #{}),
  Bin = xwamp_codec:encode(M, msgpack),
  authenticate = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
goodbye_msgpack_test(_) ->
  M = xwamp_message_builder:goodbye(
    #{<<"message">> => <<"The host is shutting down now.">>},
    <<"wamp.error.system_shutdown">>
  ),
  Bin = xwamp_codec:encode(M, msgpack),
  goodbye = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
error_msgpack_test(_) ->
  M = xwamp_message_builder:error(0, 1, #{}, <<"wamp.error.foo">>),
  Bin = xwamp_codec:encode(M, msgpack),
  error = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
error_msgpack_2_test(_) ->
  M = xwamp_message_builder:error(0, 1, #{}, <<"wamp.error.foo">>, []),
  Bin = xwamp_codec:encode(M, msgpack),
  error = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
error_msgpack_3_test(_) ->
  M = xwamp_message_builder:error(0, 1, #{}, <<"wamp.error.foo">>, [], #{}),
  Bin = xwamp_codec:encode(M, msgpack),
  error = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
publish_msgpack_test(_) ->
  M = xwamp_message_builder:publish(1, #{}, <<"xwamp.test.topic1">>),
  Bin = xwamp_codec:encode(M, msgpack),
  publish = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
publish_msgpack_2_test(_) ->
  M = xwamp_message_builder:publish(1, #{}, <<"xwamp.test.topic1">>, []),
  Bin = xwamp_codec:encode(M, msgpack),
  publish = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
publish_msgpack_3_test(_) ->
  M = xwamp_message_builder:publish(1, #{}, <<"xwamp.test.topic1">>, [], #{}),
  Bin = xwamp_codec:encode(M, msgpack),
  publish = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
published_msgpack_test(_) ->
  M = xwamp_message_builder:published(1, 2),
  Bin = xwamp_codec:encode(M, msgpack),
  published = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
subscribe_msgpack_test(_) ->
  M = xwamp_message_builder:subscribe(1, #{}, <<"xwamp.test.topic1">>),
  Bin = xwamp_codec:encode(M, msgpack),
  subscribe = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
subscribed_msgpack_test(_) ->
  M = xwamp_message_builder:subscribed(1, 3),
  Bin = xwamp_codec:encode(M, msgpack),
  subscribed = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
unsubscribe_msgpack_test(_) ->
  M = xwamp_message_builder:unsubscribe(1, 3),
  Bin = xwamp_codec:encode(M, msgpack),
  unsubscribe = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
unsubscribed_msgpack_test(_) ->
  M = xwamp_message_builder:unsubscribed(1),
  Bin = xwamp_codec:encode(M, msgpack),
  unsubscribed = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
event_msgpack_test(_) ->
  M = xwamp_message_builder:event(3, 2, #{}),
  Bin = xwamp_codec:encode(M, msgpack),
  event = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
event_msgpack_2_test(_) ->
  M = xwamp_message_builder:event(3, 2, #{}, []),
  Bin = xwamp_codec:encode(M, msgpack),
  event = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
event_msgpack_3_test(_) ->
  M = xwamp_message_builder:event(3, 2, #{}, [], #{}),
  Bin = xwamp_codec:encode(M, msgpack),
  event = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
call_msgpack_test(_) ->
  M = xwamp_message_builder:call(1, #{}, <<"xwamp.test.procedure1">>),
  Bin = xwamp_codec:encode(M, msgpack),
  call = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
call_msgpack_2_test(_) ->
  M = xwamp_message_builder:call(1, #{}, <<"xwamp.test.procedure1">>, []),
  Bin = xwamp_codec:encode(M, msgpack),
  call = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
call_msgpack_3_test(_) ->
  M = xwamp_message_builder:call(1, #{}, <<"xwamp.test.procedure1">>, [], #{}),
  Bin = xwamp_codec:encode(M, msgpack),
  call = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
cancel_msgpack_test(_) ->
  M = xwamp_message_builder:cancel(1, #{}),
  Bin = xwamp_codec:encode(M, msgpack),
  cancel = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
result_msgpack_test(_) ->
  M = xwamp_message_builder:result(1, #{}),
  Bin = xwamp_codec:encode(M, msgpack),
  result = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
result_msgpack_2_test(_) ->
  M = xwamp_message_builder:result(1, #{}, []),
  Bin = xwamp_codec:encode(M, msgpack),
  result = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
result_msgpack_3_test(_) ->
  M = xwamp_message_builder:result(1, #{}, [], #{}),
  Bin = xwamp_codec:encode(M, msgpack),
  result = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
register_msgpack_test(_) ->
  M = xwamp_message_builder:register(1, #{}, <<"xwamp.test.procedure1">>),
  Bin = xwamp_codec:encode(M, msgpack),
  register = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
registered_msgpack_2_test(_) ->
  M = xwamp_message_builder:registered(1, 4),
  Bin = xwamp_codec:encode(M, msgpack),
  registered = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
unregister_msgpack_3_test(_) ->
  M = xwamp_message_builder:unregister(1, 4),
  Bin = xwamp_codec:encode(M, msgpack),
  unregister = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
unregistered_msgpack_test(_) ->
  M = xwamp_message_builder:unregistered(1),
  Bin = xwamp_codec:encode(M, msgpack),
  unregistered = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
invocation_msgpack_test(_) ->
  M = xwamp_message_builder:invocation(1, 4, #{}),
  Bin = xwamp_codec:encode(M, msgpack),
  invocation = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
invocation_msgpack_2_test(_) ->
  M = xwamp_message_builder:invocation(1, 4, #{}, []),
  Bin = xwamp_codec:encode(M, msgpack),
  invocation = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
invocation_msgpack_3_test(_) ->
  M = xwamp_message_builder:invocation(1, 4, #{}, [], #{}),
  Bin = xwamp_codec:encode(M, msgpack),
  invocation = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
interrupt_msgpack_test(_) ->
  M = xwamp_message_builder:interrupt(1, #{}),
  Bin = xwamp_codec:encode(M, msgpack),
  interrupt = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
yield_msgpack_test(_) ->
  M = xwamp_message_builder:yield(1, #{}),
  Bin = xwamp_codec:encode(M, msgpack),
  yield = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
yield_msgpack_2_test(_) ->
  M = xwamp_message_builder:yield(1, #{}, []),
  Bin = xwamp_codec:encode(M, msgpack),
  yield = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
yield_msgpack_3_test(_) ->
  M = xwamp_message_builder:yield(1, #{}, [], #{}),
  Bin = xwamp_codec:encode(M, msgpack),
  yield = xwamp_codec:decode_message_name(Bin, msgpack),
  [M] = xwamp_codec:decode(Bin, msgpack).

%% =============================================================================
%% BERT
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
hello_bert_test(_) ->
  M = xwamp_message_builder:hello(<<"realm1">>, #{
    <<"roles">> => #{
      <<"caller">> => #{}
    }}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
welcome_bert_test(_) ->
  M = xwamp_message_builder:welcome(1, #{
    <<"realm">> => <<"realm1">>,
    <<"authid">> => <<"foo">>,
    <<"authrole">> => <<"default">>,
    <<"roles">> => #{
      <<"dealer">> => #{},
      <<"broker">> => #{}
    }}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
abort_bert_test(_) ->
  M = xwamp_message_builder:abort(#{<<"message">> => <<"foo">>}, <<"wamp.error.foo">>),
  ?assertMatch(
    [M],
    xwamp_codec:decode(xwamp_codec:encode(M, bert), bert)
  ).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
challenge_bert_test(_) ->
  M = xwamp_message_builder:challenge(<<"foo">>, #{}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
authenticate_bert_test(_) ->
  M = xwamp_message_builder:authenticate(<<"foo">>, #{}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
goodbye_bert_test(_) ->
  M = xwamp_message_builder:goodbye(
    #{<<"message">> => <<"The host is shutting down now.">>},
    <<"wamp.error.system_shutdown">>
  ),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
error_bert_test(_) ->
  M = xwamp_message_builder:error(0, 1, #{}, <<"wamp.error.foo">>),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
error_bert_2_test(_) ->
  M = xwamp_message_builder:error(0, 1, #{}, <<"wamp.error.foo">>, []),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
error_bert_3_test(_) ->
  M = xwamp_message_builder:error(0, 1, #{}, <<"wamp.error.foo">>, [], #{}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
publish_bert_test(_) ->
  M = xwamp_message_builder:publish(1, #{}, <<"xwamp.test.topic1">>),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
publish_bert_2_test(_) ->
  M = xwamp_message_builder:publish(1, #{}, <<"xwamp.test.topic1">>, []),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
publish_bert_3_test(_) ->
  M = xwamp_message_builder:publish(1, #{}, <<"xwamp.test.topic1">>, [], #{}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
published_bert_test(_) ->
  M = xwamp_message_builder:published(1, 2),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
subscribe_bert_test(_) ->
  M = xwamp_message_builder:subscribe(1, #{}, <<"xwamp.test.topic1">>),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
subscribed_bert_test(_) ->
  M = xwamp_message_builder:subscribed(1, 3),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
unsubscribe_bert_test(_) ->
  M = xwamp_message_builder:unsubscribe(1, 3),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
unsubscribed_bert_test(_) ->
  M = xwamp_message_builder:unsubscribed(1),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
event_bert_test(_) ->
  M = xwamp_message_builder:event(3, 2, #{}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
event_bert_2_test(_) ->
  M = xwamp_message_builder:event(3, 2, #{}, []),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
event_bert_3_test(_) ->
  M = xwamp_message_builder:event(3, 2, #{}, [], #{}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
call_bert_test(_) ->
  M = xwamp_message_builder:call(1, #{}, <<"xwamp.test.procedure1">>),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
call_bert_2_test(_) ->
  M = xwamp_message_builder:call(1, #{}, <<"xwamp.test.procedure1">>, []),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
call_bert_3_test(_) ->
  M = xwamp_message_builder:call(1, #{}, <<"xwamp.test.procedure1">>, [], #{}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
cancel_bert_test(_) ->
  M = xwamp_message_builder:cancel(1, #{}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
result_bert_test(_) ->
  M = xwamp_message_builder:result(1, #{}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
result_bert_2_test(_) ->
  M = xwamp_message_builder:result(1, #{}, []),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
result_bert_3_test(_) ->
  M = xwamp_message_builder:result(1, #{}, [], #{}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
result_bert_4_test(_) ->
  M = xwamp_message_builder:result(1, #{}, [true], #{}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
result_bert_5_test(_) ->
  M = xwamp_message_builder:result(1, #{}, [false], #{}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
result_bert_6_test(_) ->
  M = xwamp_message_builder:result(1, #{}, [undefined], #{}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
result_bert_7_test(_) ->
  M = xwamp_message_builder:result(1, #{}, [#{foo => true, bar => baz}], #{}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
register_bert_test(_) ->
  M = xwamp_message_builder:register(1, #{}, <<"xwamp.test.procedure1">>),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
registered_bert_2_test(_) ->
  M = xwamp_message_builder:registered(1, 4),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
unregister_bert_3_test(_) ->
  M = xwamp_message_builder:unregister(1, 4),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
unregistered_bert_test(_) ->
  M = xwamp_message_builder:unregistered(1),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
invocation_bert_test(_) ->
  M = xwamp_message_builder:invocation(1, 4, #{}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
invocation_bert_2_test(_) ->
  M = xwamp_message_builder:invocation(1, 4, #{}, []),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
invocation_bert_3_test(_) ->
  M = xwamp_message_builder:invocation(1, 4, #{}, [], #{}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
interrupt_bert_test(_) ->
  M = xwamp_message_builder:interrupt(1, #{}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
yield_bert_test(_) ->
  M = xwamp_message_builder:yield(1, #{}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
yield_bert_2_test(_) ->
  M = xwamp_message_builder:yield(1, #{}, []),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
yield_bert_3_test(_) ->
  M = xwamp_message_builder:yield(1, #{}, [], #{}),
  [M] = xwamp_codec:decode(xwamp_codec:encode(M, bert), bert).

%% =============================================================================
%% ERL
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
hello_erl_test(_) ->
  M = xwamp_message_builder:hello(<<"realm1">>, #{
    <<"roles">> => #{
      <<"caller">> => #{}
    }}),
  Bin = xwamp_codec:encode(M, erl),
  hello = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
welcome_erl_test(_) ->
  M = xwamp_message_builder:welcome(1, #{
    <<"realm">> => <<"realm1">>,
    <<"authid">> => <<"foo">>,
    <<"authrole">> => <<"default">>,
    <<"roles">> => #{
      <<"dealer">> => #{},
      <<"broker">> => #{}
    }}),
  Bin = xwamp_codec:encode(M, erl),
  welcome = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
welcome_erl_test_2(_) ->
  M = xwamp_message_builder:welcome(1, #{
    roles => #{
      dealer => #{},
      broker => #{}
    }}),
  Bin = xwamp_codec:encode(M, erl),
  welcome = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
abort_erl_test(_) ->
  M = xwamp_message_builder:abort(#{<<"message">> => <<"foo">>}, <<"wamp.error.foo">>),
  Bin = xwamp_codec:encode(M, erl),
  abort = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
challenge_erl_test(_) ->
  M = xwamp_message_builder:challenge(<<"foo">>, #{}),
  Bin = xwamp_codec:encode(M, erl),
  challenge = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
authenticate_erl_test(_) ->
  M = xwamp_message_builder:authenticate(<<"foo">>, #{}),
  Bin = xwamp_codec:encode(M, erl),
  authenticate = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
goodbye_erl_test(_) ->
  M = xwamp_message_builder:goodbye(
    #{<<"message">> => <<"The host is shutting down now.">>},
    <<"wamp.error.system_shutdown">>
  ),
  Bin = xwamp_codec:encode(M, erl),
  goodbye = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
error_erl_test(_) ->
  M = xwamp_message_builder:error(0, 1, #{}, <<"wamp.error.foo">>),
  Bin = xwamp_codec:encode(M, erl),
  error = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
error_erl_2_test(_) ->
  M = xwamp_message_builder:error(0, 1, #{}, <<"wamp.error.foo">>, []),
  Bin = xwamp_codec:encode(M, erl),
  error = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
error_erl_3_test(_) ->
  M = xwamp_message_builder:error(0, 1, #{}, <<"wamp.error.foo">>, [], #{}),
  Bin = xwamp_codec:encode(M, erl),
  error = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
publish_erl_test(_) ->
  M = xwamp_message_builder:publish(1, #{}, <<"xwamp.test.topic1">>),
  Bin = xwamp_codec:encode(M, erl),
  publish = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
publish_erl_2_test(_) ->
  M = xwamp_message_builder:publish(1, #{}, <<"xwamp.test.topic1">>, []),
  Bin = xwamp_codec:encode(M, erl),
  publish = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
publish_erl_3_test(_) ->
  M = xwamp_message_builder:publish(1, #{}, <<"xwamp.test.topic1">>, [], #{}),
  Bin = xwamp_codec:encode(M, erl),
  publish = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
published_erl_test(_) ->
  M = xwamp_message_builder:published(1, 2),
  Bin = xwamp_codec:encode(M, erl),
  published = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
subscribe_erl_test(_) ->
  M = xwamp_message_builder:subscribe(1, #{}, <<"xwamp.test.topic1">>),
  Bin = xwamp_codec:encode(M, erl),
  subscribe = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
subscribed_erl_test(_) ->
  M = xwamp_message_builder:subscribed(1, 3),
  Bin = xwamp_codec:encode(M, erl),
  subscribed = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
unsubscribe_erl_test(_) ->
  M = xwamp_message_builder:unsubscribe(1, 3),
  Bin = xwamp_codec:encode(M, erl),
  unsubscribe = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
unsubscribed_erl_test(_) ->
  M = xwamp_message_builder:unsubscribed(1),
  Bin = xwamp_codec:encode(M, erl),
  unsubscribed = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
event_erl_test(_) ->
  M = xwamp_message_builder:event(3, 2, #{}),
  Bin = xwamp_codec:encode(M, erl),
  event = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
event_erl_2_test(_) ->
  M = xwamp_message_builder:event(3, 2, #{}, []),
  Bin = xwamp_codec:encode(M, erl),
  event = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
event_erl_3_test(_) ->
  M = xwamp_message_builder:event(3, 2, #{}, [], #{}),
  Bin = xwamp_codec:encode(M, erl),
  event = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
call_erl_test(_) ->
  M = xwamp_message_builder:call(1, #{}, <<"xwamp.test.procedure1">>),
  Bin = xwamp_codec:encode(M, erl),
  call = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
call_erl_2_test(_) ->
  M = xwamp_message_builder:call(1, #{}, <<"xwamp.test.procedure1">>, []),
  Bin = xwamp_codec:encode(M, erl),
  call = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
call_erl_3_test(_) ->
  M = xwamp_message_builder:call(1, #{}, <<"xwamp.test.procedure1">>, [], #{}),
  Bin = xwamp_codec:encode(M, erl),
  call = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
call_erl_4_test(_) ->
  Args = [
    [{bar, self()}]
  ],
  M0 = xwamp_message_builder:call(1, #{}, <<"foo">>, Args, #{}),
  Bin = xwamp_codec:encode(M0, erl),
  call = xwamp_codec:decode_message_name(Bin, erl),
  [M1] = xwamp_codec:decode(Bin, erl),
  ?assertMatch(
    {call, 1, _, <<"foo">>, [#{<<"bar">> := _}],
    undefined},
    M1
  ).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
cancel_erl_test(_) ->
  M = xwamp_message_builder:cancel(1, #{}),
  Bin = xwamp_codec:encode(M, erl),
  cancel = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
result_erl_test(_) ->
  M = xwamp_message_builder:result(1, #{}),
  Bin = xwamp_codec:encode(M, erl),
  result = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
result_erl_2_test(_) ->
  M = xwamp_message_builder:result(1, #{}, []),
  Bin = xwamp_codec:encode(M, erl),
  result = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
result_erl_3_test(_) ->
  M = xwamp_message_builder:result(1, #{}, [], #{}),
  Bin = xwamp_codec:encode(M, erl),
  result = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
result_erl_4_test(_) ->
  Arg = #{
    a => undefined,
    b => true,
    c => false,
    d => hello,
    e => #{
      a => undefined,
      b => true,
      c => false,
      d => hello,
      e => #{
        a => undefined,
        b => true,
        c => false,
        d => hello
      }
    }
  },
  M = xwamp_message_builder:result(1, #{}, [Arg], #{}),
  Bin = xwamp_codec:encode(M, erl),
  ExpectedArg = #{
    <<"a">> => undefined,
    <<"b">> => true,
    <<"c">> => false,
    <<"d">> => <<"hello">>,
    <<"e">> => #{
      <<"a">> => undefined,
      <<"b">> => true,
      <<"c">> => false,
      <<"d">> => <<"hello">>,
      <<"e">> => #{
        <<"a">> => undefined,
        <<"b">> => true,
        <<"c">> => false,
        <<"d">> => <<"hello">>
      }
    }
  },
  [{result, 1, #{}, [ResultArg], undefined}] = xwamp_codec:decode(Bin, erl),
  ?assertEqual(
    ExpectedArg,
    ResultArg
  ).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
register_erl_test(_) ->
  M = xwamp_message_builder:register(1, #{}, <<"xwamp.test.procedure1">>),
  Bin = xwamp_codec:encode(M, erl),
  register = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
registered_erl_2_test(_) ->
  M = xwamp_message_builder:registered(1, 4),
  Bin = xwamp_codec:encode(M, erl),
  registered = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
unregister_erl_3_test(_) ->
  M = xwamp_message_builder:unregister(1, 4),
  Bin = xwamp_codec:encode(M, erl),
  unregister = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
unregistered_erl_test(_) ->
  M = xwamp_message_builder:unregistered(1),
  Bin = xwamp_codec:encode(M, erl),
  unregistered = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
invocation_erl_test(_) ->
  M = xwamp_message_builder:invocation(1, 4, #{}),
  Bin = xwamp_codec:encode(M, erl),
  invocation = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
invocation_erl_2_test(_) ->
  M = xwamp_message_builder:invocation(1, 4, #{}, []),
  Bin = xwamp_codec:encode(M, erl),
  invocation = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
invocation_erl_3_test(_) ->
  M = xwamp_message_builder:invocation(1, 4, #{}, [], #{}),
  Bin = xwamp_codec:encode(M, erl),
  invocation = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
interrupt_erl_test(_) ->
  M = xwamp_message_builder:interrupt(1, #{}),
  Bin = xwamp_codec:encode(M, erl),
  interrupt = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
yield_erl_test(_) ->
  M = xwamp_message_builder:yield(1, #{}),
  Bin = xwamp_codec:encode(M, erl),
  yield = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
yield_erl_2_test(_) ->
  M = xwamp_message_builder:yield(1, #{}, []),
  Bin = xwamp_codec:encode(M, erl),
  yield = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
yield_erl_3_test(_) ->
  M = xwamp_message_builder:yield(1, #{}, [], #{}),
  Bin = xwamp_codec:encode(M, erl),
  yield = xwamp_codec:decode_message_name(Bin, erl),
  [M] = xwamp_codec:decode(Bin, erl).

%% =============================================================================
%% EXTENSION KEYS VALIDATION
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
validate_extension_key_1_test(_) ->
  Keys = [
    '_xyz1',
    '_xyz2',
    '_xyz3',
    %% This should be ignored
    '_foo',
    'bar'
  ],
  Allowed = [{yield, Keys}],

	application:set_env(xwamp, extended_options, Allowed, [{persistent, false}]),
	
	{ok, [{yield, Keys}]} = application:get_env(xwamp, extended_options),

  Expected = #{
    <<"ppt_cipher">> => <<"xsalsa20poly1305">>,
		<<"ppt_keyid">> => <<"ppt_keyid">>
  },

  Opts = #{
		<<"ppt_cipher">> => <<"xsalsa20poly1305">>,
		<<"ppt_keyid">> => <<"ppt_keyid">>,
    <<"_xyz1">> => 1,
    <<"_xyz2">> => 2,
    <<"_xyz3">> => 3,
    <<"foo">> => 1,
    <<"_bar">> => 1,
    <<"_x">> => 1
  },

  M1 = xwamp_message_builder:yield(1, Opts),
  ?assertEqual(Expected, xwamp_options:options(M1)),

  M2 = xwamp_message_builder:cancel(1, Opts),
  ?assertEqual(maps:new(), xwamp_options:options(M2)).