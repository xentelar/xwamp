%% -----------------------------------------------------------------------------
%%
%% Copyright (c) 2025-2025 Xentelar Advanced Technologies. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% @doc xwamp_codec public API
%% @end
%% -----------------------------------------------------------------------------
-module(xwamp_codec).

-include_lib("kernel/include/logger.hrl").

-include("xwamp.hrl").

-export([encode/2]).
-export([encode/3]).

-export([decode/2]).
-export([decode/3]).

-export([message_name/1]).
-export([decode_message_name/2]).

%% =============================================================================
%% API
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec decode_message_name(binary(), codec()) ->
  message_name() | no_return().
decode_message_name(Data, Codec) ->
  do_decode_message_name(Data, Codec).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec decode(Data :: binary(), Codec :: codec()) -> 
                {Messages :: [wamp_message()], Rest :: binary()} | no_return().
decode(Data, json = Codec) ->
  decode_text(Data, Codec, opts(Codec, decode), []);

decode(Data, cbor = Codec) ->
  decode_binary(Data, Codec, opts(Codec, decode), []);

decode(Data, msgpack = Codec) ->
  decode_binary(Data, Codec, opts(Codec, decode), []);

decode(Data, bert = Codec) ->
  decode_binary(Data, Codec, opts(Codec, decode), []);

decode(Data, erl = Codec) ->
  decode_binary(Data, Codec, opts(Codec, decode), []);

decode(_Data, Format) ->
  error({unsupported_codec, Format}).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec decode(Data :: binary(), Codec :: codec(), Opts :: list()) ->
                {Messages :: [wamp_message()], Rest :: binary()} | no_return().
decode(Data, json = Codec, Opts) ->
  decode_text(Data, Codec, Opts, []);

decode(Data, cbor = Codec, Opts) ->
  decode_binary(Data, Codec, Opts, []);

decode(Data, msgpack = Codec, Opts) ->
  decode_binary(Data, Codec, Opts, []);

decode(Data, bert = Codec, Opts) ->
  decode_binary(Data, Codec, Opts, []);

decode(Data, erl = Codec, Opts) ->
  decode_binary(Data, Codec, Opts, []);

decode(_Data, Format, _Opts) ->
  error({unsupported_codec, Format}).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec encode(wamp_message() | list(), codec()) -> binary() | no_return().
encode(Message, Codec) when is_tuple(Message) ->
  encode(pack(Message), Codec);

encode(Message, json = Codec) when is_list(Message) ->
  encode(Message, Codec, opts(Codec, encode));

encode(Message, cbor = Codec) when is_list(Message) ->
  encode(Message, cbor, opts(Codec, encode));

encode(Message, msgpack = Codec) when is_list(Message) ->
  encode(Message, msgpack, opts(Codec, encode));

encode(Message, bert = Codec) when is_list(Message) ->
  encode(Message, bert, opts(Codec, encode));

encode(Message, erl = Codec) when is_list(Message) ->
  encode(Message, Codec, opts(Codec, encode));

encode(Message, Format) when is_list(Message) ->
  error({unsupported_codec, Format}).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec encode(wamp_message() | list(), codec(), Opts :: list()) ->
                                                binary() | no_return().
encode(Message, Codec, Opts) when is_tuple(Message) ->
  encode(pack(Message), Codec, Opts);

encode(Message, json, _Opts) when is_list(Message) ->
  thoas:encode(Message);
  %iolist_to_binary(json:encode(Message));

encode(Message, msgpack, Opts) when is_list(Message) ->
  msgpack:pack(Message, Opts);

encode(Message, cbor, _Opts) when is_list(Message) ->
  error("not implemented");

encode(Message, erl, Opts) when is_list(Message) ->
  term_to_binary(xwamp_erl_codec:encode(Message, Opts));

encode(Message, bert, _) when is_list(Message) ->
  bert:encode(Message);

encode(Message, Format, _) when is_list(Message) ->
  error({unsupported_codec, Format}).

%% -----------------------------------------------------------------------------
%% @doc
%% Returns a message in WAMP list format.
%% @end
%% -----------------------------------------------------------------------------
-spec pack(wamp_message()) -> list() | no_return().
pack(#error{} = M) ->
  #error{
    request_type = ReqType,
    request_id = ReqId,
    details = Details,
    error_uri = ErrorUri,
    args = Args,
    kwargs = KWArgs
  } = M,
  T = pack_optionals(Args, KWArgs, Details),
  [?ERROR, ReqType, ReqId, Details, ErrorUri | T];

pack(#publish{} = M) ->
  #publish{
    request_id = ReqId,
    options = Options,
    topic_uri = TopicUri,
    args = Args,
    kwargs = KWArgs
  } = M,
  T = pack_optionals(Args, KWArgs, Options),
  [?PUBLISH, ReqId, Options, TopicUri | T];

pack(#event{} = M) ->
  #event{
    subscription_id = SubsId,
    publication_id = PubId,
    details = Details,
    args = Args,
    kwargs = KWArgs
  } = M,
  T = pack_optionals(Args, KWArgs, Details),
  [?EVENT, SubsId, PubId, Details | T];

pack(#call{} = M) ->
  #call{
    request_id = ReqId,
    options = Options,
    procedure_uri = ProcedureUri,
    args = Args,
    kwargs = KWArgs
  } = M,
  T = pack_optionals(Args, KWArgs, Options),
  [?CALL, ReqId, Options, ProcedureUri | T];

pack(#result{} = M) ->
  #result{
    request_id = ReqId,
    details = Details,
    args = Args,
    kwargs = KWArgs
  } = M,
  T = pack_optionals(Args, KWArgs, Details),
  [?RESULT, ReqId, Details | T];

pack(#invocation{} = M) ->
  #invocation{
    request_id = ReqId,
    registration_id = RegId,
    details = Details,
    args = Args,
    kwargs = KWArgs
  } = M,
  T = pack_optionals(Args, KWArgs, Details),
  [?INVOCATION, ReqId, RegId, Details | T];

pack(#yield{} = M) ->
  #yield{
    request_id = ReqId,
    options = Options,
    args = Args,
    kwargs = KWArgs
  } = M,
  T = pack_optionals(Args, KWArgs, Options),
  [?YIELD, ReqId, Options | T];

pack(#unregistered{request_id = ReqId, details = undefined}) ->
  [?UNREGISTERED, ReqId];

pack(#unregistered{request_id = ReqId, details = Details}) ->
  [?UNREGISTERED, ReqId, Details];

pack(#hello{} = M) -> pack_generic(?HELLO, M);
pack(#welcome{} = M) -> pack_generic(?WELCOME, M);
pack(#abort{} = M) -> pack_generic(?ABORT, M);
pack(#challenge{} = M) -> pack_generic(?CHALLENGE, M);
pack(#authenticate{} = M) -> pack_generic(?AUTHENTICATE, M);
pack(#goodbye{} = M) -> pack_generic(?GOODBYE, M);
pack(#published{} = M) -> pack_generic(?PUBLISHED, M);
pack(#subscribe{} = M) -> pack_generic(?SUBSCRIBE, M);
pack(#subscribed{} = M) -> pack_generic(?SUBSCRIBED, M);
pack(#unsubscribe{} = M) -> pack_generic(?UNSUBSCRIBE, M);
pack(#unsubscribed{} = M) -> pack_generic(?UNSUBSCRIBED, M);
pack(#cancel{} = M) -> pack_generic(?CANCEL, M);
pack(#register{} = M) -> pack_generic(?REGISTER, M);
pack(#registered{} = M) -> pack_generic(?REGISTERED, M);
pack(#unregister{} = M) -> pack_generic(?UNREGISTER, M);
pack(#interrupt{} = M) -> pack_generic(?INTERRUPT, M);

pack(_) ->
  error(badarg).

%% -----------------------------------------------------------------------------
%% @doc
%% All other message types are straight forward
%% @end
%% -----------------------------------------------------------------------------
-spec pack_generic(term(), tuple()) -> list().
pack_generic(Type, M) when is_tuple(M) ->
  [_H|T] = tuple_to_list(M),
  [Type | T].

%% -----------------------------------------------------------------------------
%% @doc
%% Converts a message from a WAMP list external format to
%% an internal format (erlang record).
%% See {@link xwamp_message_builder} for all message types.
%% @end
%% -----------------------------------------------------------------------------
-spec unpack(list()) -> wamp_message() | no_return().
unpack([?HELLO, RealmUri, Details]) ->
  xwamp_message_builder:hello(RealmUri, Details);

unpack([?WELCOME, SessionId, Details]) ->
  xwamp_message_builder:welcome(SessionId, Details);

unpack([?CHALLENGE, AuthMethod, Extra]) ->
  xwamp_message_builder:challenge(AuthMethod, Extra);

unpack([?AUTHENTICATE, Signature, Extra]) ->
  xwamp_message_builder:authenticate(Signature, Extra);

unpack([?ABORT, Details, ReasonUri]) ->
  xwamp_message_builder:abort(Details, ReasonUri);

unpack([?GOODBYE, Details, ReasonUri]) ->
  xwamp_message_builder:goodbye(Details, ReasonUri);

unpack([?ERROR, ReqType, ReqId, Details, ErrorUri]) ->
  xwamp_message_builder:error(
    ReqType,
    ReqId,
    Details,
    ErrorUri
  );

unpack([?ERROR, ReqType, ReqId, Details, 
          ErrorUri, Payload]) when is_binary(Payload) ->
  xwamp_message_builder:error(
    ReqType,
    ReqId,
    Details,
    ErrorUri,
    Payload
  );

unpack([?ERROR, ReqType, ReqId, Details, 
          ErrorUri, Args]) when is_list(Args) ->
  xwamp_message_builder:error(
    ReqType,
    ReqId,
    Details,
    ErrorUri,
    Args
  );

unpack([?ERROR, ReqType, ReqId, Details, 
          ErrorUri, Args, KWArgs]) when is_list(Args), is_map(KWArgs) ->
  xwamp_message_builder:error(
    ReqType,
    ReqId,
    Details,
    ErrorUri,
    Args,
    KWArgs
  );

unpack([?PUBLISH, ReqId, Options, TopicUri]) ->
  xwamp_message_builder:publish(ReqId, Options, TopicUri);

unpack([?PUBLISH, ReqId, Options, TopicUri, Args]) ->
  xwamp_message_builder:publish(
    ReqId,
    Options,
    TopicUri,
    Args
  );

unpack([?PUBLISH, ReqId, Options, TopicUri, Args, KWArgs]) ->
  xwamp_message_builder:publish(
    ReqId,
    Options,
    TopicUri,
    Args,
    KWArgs
  );

unpack([?PUBLISHED, ReqId, PubId]) ->
  xwamp_message_builder:published(ReqId, PubId);

unpack([?SUBSCRIBE, ReqId, Options, TopicUri]) ->
  xwamp_message_builder:subscribe(ReqId, Options, TopicUri);

unpack([?SUBSCRIBED, ReqId, SubsId]) ->
  xwamp_message_builder:subscribed(ReqId, SubsId);

unpack([?UNSUBSCRIBE, ReqId, SubsId]) ->
  xwamp_message_builder:unsubscribe(ReqId, SubsId);

unpack([?UNSUBSCRIBED, ReqId]) ->
  xwamp_message_builder:unsubscribed(ReqId);

unpack([?EVENT, SubsId, PubId, Details]) ->
  xwamp_message_builder:event(
    SubsId,
    PubId,
    Details
  );

unpack([?EVENT, SubsId, PubId, Details, Args]) ->
  xwamp_message_builder:event(
    SubsId,
    PubId,
    Details,
    Args
  );

unpack([?EVENT, SubsId, PubId, Details, Args, KWArgs]) ->
  xwamp_message_builder:event(
    SubsId,
    PubId,
    Details,
    Args,
    KWArgs
  );

unpack([?CALL, ReqId, Options, ProcedureUri]) ->
  xwamp_message_builder:call(
    ReqId,
    Options,
    ProcedureUri
  );

unpack([?CALL, ReqId, Options, ProcedureUri, Args]) ->
  xwamp_message_builder:call(
    ReqId,
    Options,
    ProcedureUri,
    Args
  );

unpack([?CALL, ReqId, Options, ProcedureUri, Args, KWArgs]) ->
  xwamp_message_builder:call(
    ReqId,
    Options,
    ProcedureUri,
    Args,
    KWArgs
  );

unpack([?CANCEL, ReqId, Options]) ->
  xwamp_message_builder:cancel(ReqId, Options);

unpack([?INTERRUPT, ReqId, Options]) ->
  xwamp_message_builder:interrupt(ReqId, Options);

unpack([?RESULT, ReqId, Details]) ->
  xwamp_message_builder:result(ReqId, Details);

unpack([?RESULT, ReqId, Details, Args]) ->
  xwamp_message_builder:result(ReqId, Details, Args);

unpack([?RESULT, ReqId, Details, Args, KWArgs]) ->
  xwamp_message_builder:result(ReqId, Details, Args, KWArgs);


unpack([?REGISTER, ReqId, Options, ProcedureUri]) ->
  xwamp_message_builder:register(ReqId, Options, ProcedureUri);

unpack([?REGISTERED, ReqId, RegId]) ->
  xwamp_message_builder:registered(ReqId, RegId);

unpack([?UNREGISTER, ReqId, RegId]) ->
  xwamp_message_builder:unregister(ReqId, RegId);

unpack([?UNREGISTERED, ReqId]) ->
  xwamp_message_builder:unregistered(ReqId);

unpack([?UNREGISTERED, ReqId, Details]) ->
  xwamp_message_builder:unregistered(ReqId, Details);

unpack([?INVOCATION, ReqId, RegId, Details]) ->
  xwamp_message_builder:invocation(
    ReqId,
    RegId,
    Details
  );

unpack([?INVOCATION, ReqId, RegId, Details, Args]) ->
  xwamp_message_builder:invocation(
    ReqId,
    RegId,
    Details,
    Args
  );

unpack([?INVOCATION, ReqId, RegId, Details, Args, KWArgs]) ->
  xwamp_message_builder:invocation(
    ReqId,
    RegId,
    Details,
    Args,
    KWArgs
  );

unpack([?YIELD, ReqId, Options]) ->
  xwamp_message_builder:yield(
    ReqId,
    Options
  );

unpack([?YIELD, ReqId, Options, Args]) ->
  xwamp_message_builder:yield(
    ReqId,
    Options,
    Args
  );

unpack([?YIELD, ReqId, Options, Args, KWArgs]) ->
  xwamp_message_builder:yield(
    ReqId,
    Options,
    Args,
    KWArgs
  );

unpack(M) ->
  error({invalid_message, M}).

%% =============================================================================
%% PRIVATE
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec opts(atom(), atom()) -> list().
opts(erl, encode) ->
  [];%xwamp_cfg:serialization_erl_encode();

opts(erl, decode) ->
  [];%xwamp_cfg:serialization_erl_decode());

opts(cbor, encode) ->
  [];%xwamp_cfg:serialization_cbor_encode();

opts(cbor, decode) ->
  [];%xwamp_cfg:serialization_cbor_decode());

opts(bert, _) ->
  [];

opts(json, encode) ->
  [];%xwamp_cfg:serialization_json_encode());

opts(json, decode) ->
  [];%xwamp_cfg:serialization_json_decode());

opts(msgpack, encode) ->
  [{map_format, map}, {pack_str, from_binary}];

opts(msgpack, decode) ->
  [{map_format, map}, {unpack_str, as_binary}].

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec do_decode_message_name(binary(), atom()) -> atom().
do_decode_message_name(<<131, 108, _:32, 97, Type, _/binary>>, erl) ->
  message_name(Type);

do_decode_message_name(<<131, 107, _Len:16, Type, _/binary>>, erl) ->
  %% When all elements in the list are integers, erlang encodes it
  %% with 107 :: string type
  message_name(Type);

do_decode_message_name(_, erl) ->
  error(badarg);

do_decode_message_name(Data, cbor) ->
  do_decode_message_name(Data, erl);

do_decode_message_name(Data, bert) ->
  do_decode_message_name(Data, erl);

do_decode_message_name(<<"[", Rest/binary>>, json) ->
  case binary:match(Rest, [<< $, >>], []) of
    nomatch ->
      error(badarg);
    {Pos, 1} ->
      Type = binary:part(Rest, {0, Pos}),
      message_name(binary_to_integer(Type))
  end;

do_decode_message_name(_, json) ->
  error(badarg);

do_decode_message_name(<<2#101:3, _:5, 0:1, V:7, _/binary>>, msgpack) ->
  message_name(V);

do_decode_message_name(<<2#101:3, _:5, 16#CD, V:8, _/binary>>, msgpack) ->
  message_name(V);

do_decode_message_name(<<2#1001:4, _:4, 0:1, V:7, _/binary>>, msgpack) ->
  message_name(V);

do_decode_message_name(
  <<2#1001:4, _:4, 16#CD, V:8/unsigned-integer, _/binary>>, msgpack) ->
  %% We use msgpack list with len < 16
  message_name(V);

do_decode_message_name(_, msgpack) ->
  error(badarg);

do_decode_message_name(_, Enc) ->
  error({unsupported_encoding, Enc}).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec decode_text(binary(), codec(), Opts :: list(), Acc0 :: [wamp_message()]) ->
                                    {Acc1 :: [wamp_message()], Buffer :: binary()} | no_return().
decode_text(Data, json, Opts, Acc) ->
  decode_message(Data, json, Opts, Acc).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec decode_binary(binary(), codec(), Opts :: list(), Acc0 :: [wamp_message()]) ->
                                    {Acc1 :: [wamp_message()], Buffer :: binary()} | no_return().
decode_binary(Data, Enc, Opts, Acc) ->
  decode_message(Data, Enc, Opts, Acc).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec decode_message(binary(), codec(), Opts :: list(), [wamp_message()]) ->
                                                                [wamp_message()] | no_return().
decode_message(Data, json, _Opts, Acc) ->
  %% Decode might failed with badarg exception if not a proper JSON
  %?LOG_INFO(#{description => "json decode", data => Data}),
  %M = json:decode(Data),
  {ok, M} = thoas:decode(Data),
  unpack(M, Acc);

decode_message(Data, msgpack, Opts, Acc) ->
  {ok, M} = msgpack:unpack(Data, Opts),
  unpack(M, Acc);

decode_message(_Data, cbor, _Opts, _Acc) ->
  error("not implemented");

decode_message(Data, bert, _, Acc) ->
  M = bert:decode(Data),
  unpack(M, Acc);

decode_message(Bin, erl, Opts, Acc) ->
  %% We use the safe option to avoid atom exhaustion
  %% Check Preventing atom exhaustion guide https://bit.ly/3X2QygH
  Term = binary_to_term(Bin, [safe]),
  M = xwamp_erl_codec:decode(Term, Opts),
  unpack(M, Acc);

decode_message(_Data, Enc, _, _Acc) ->
  error({unsupported_encoding, Enc}).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec unpack(list(), list()) -> list().
unpack(M, Acc) ->
  try
    [unpack(M) | Acc]
  catch
    error:{validation_failed, Reason} ->
      error({validation_failed, Reason, request_info(M)});
    error:{invalid_uri, Uri} ->
      error({invalid_uri, Uri, request_info(M)})
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec request_info(list()) -> map().
request_info([?HELLO, _, _]) ->
  #{request_type => ?HELLO, request_id => undefined};

request_info([?WELCOME, _, _]) ->
  #{request_type => ?WELCOME, request_id => undefined};

request_info([?CHALLENGE, _, _]) ->
  #{request_type => ?CHALLENGE, request_id => undefined};

request_info([?AUTHENTICATE, _, _]) ->
  #{request_type => ?AUTHENTICATE, request_id => undefined};

request_info([?ABORT, _, _]) ->
  #{request_type => ?ABORT, request_id => undefined};

request_info([?GOODBYE, _, _]) ->
  #{request_type => ?GOODBYE, request_id => undefined};

request_info([?ERROR | _])->
  #{request_type => ?ERROR, request_id => undefined};

request_info([?PUBLISH, ReqId | _]) ->
  #{request_type => ?PUBLISH, request_id => ReqId};

request_info([?PUBLISHED, ReqId | _]) ->
  #{request_type => ?PUBLISHED, request_id => ReqId};

request_info([?SUBSCRIBE, ReqId | _]) ->
  #{request_type => ?SUBSCRIBE, request_id => ReqId};

request_info([?SUBSCRIBED, ReqId | _]) ->
  #{request_type => ?SUBSCRIBED, request_id => ReqId};

request_info([?UNSUBSCRIBE, ReqId | _]) ->
  #{request_type => ?UNSUBSCRIBE, request_id => ReqId};

request_info([?UNSUBSCRIBED, ReqId]) ->
  #{request_type => ?UNSUBSCRIBED, request_id => ReqId};

request_info([?EVENT | _]) ->
  #{request_type => ?EVENT, request_id => undefined};

request_info([?EVENT_RECEIVED | _]) ->
  #{request_type => ?EVENT_RECEIVED, request_id => undefined};

request_info([?SUBSCRIBER_RECEIVED | _]) ->
  #{request_type => ?SUBSCRIBER_RECEIVED, request_id => undefined};

request_info([?CALL, ReqId | _]) ->
  #{request_type => ?CALL, request_id => ReqId};

request_info([?CANCEL, ReqId | _]) ->
  #{request_type => ?CANCEL, request_id => ReqId};

request_info([?INTERRUPT, ReqId | _]) ->
  #{request_type => ?INTERRUPT, request_id => ReqId};

request_info([?RESULT, ReqId | _]) ->
  #{request_type => ?RESULT, request_id => ReqId};

request_info([?REGISTER, ReqId | _]) ->
  #{request_type => ?REGISTER, request_id => ReqId};

request_info([?REGISTERED, ReqId | _]) ->
  #{request_type => ?REGISTERED, request_id => ReqId};

request_info([?UNREGISTER, ReqId | _]) ->
  #{request_type => ?UNREGISTER, request_id => ReqId};

request_info([?UNREGISTERED, ReqId]) ->
  #{request_type => ?UNREGISTERED, request_id => ReqId};

request_info([?INVOCATION, ReqId | _]) ->
  #{request_type => ?INVOCATION, request_id => ReqId};

request_info([?YIELD, ReqId | _]) ->
  #{request_type => ?YIELD, request_id => ReqId}.

%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% RFC: https://wamp-proto.org/wamp_latest_ietf.html#name-empty-arguments-and-keyword
%%  - Implementations SHOULD avoid sending empty Arguments lists.
%%  - Implementations SHOULD avoid sending empty ArgumentsKw dictionaries.
%% @end
%% -----------------------------------------------------------------------------
-spec pack_optionals(term(), term(), term()) -> list().
pack_optionals(undefined, undefined, _) ->
  [];

pack_optionals([], KWArgs, Details) ->
  pack_optionals(undefined, KWArgs, Details);

pack_optionals(Args, KWArgs, Details) when map_size(KWArgs) =:= 0 ->
  pack_optionals(Args, undefined, Details);

pack_optionals(undefined, KWArgs, _) ->
  [[], KWArgs];

pack_optionals(Args, undefined, _) ->
  [Args];

pack_optionals(Args, KWArgs, _) ->
  [Args, KWArgs].

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec message_name(1..255) -> atom().
message_name(?HELLO)                -> hello;
message_name(?WELCOME)              -> welcome;
message_name(?ABORT)                -> abort;
message_name(?CHALLENGE)            -> challenge;
message_name(?AUTHENTICATE)         -> authenticate;
message_name(?GOODBYE)              -> goodbye;
message_name(?ERROR)                -> error;
message_name(?PUBLISH)              -> publish;
message_name(?PUBLISHED)            -> published;
message_name(?SUBSCRIBE)            -> subscribe;
message_name(?SUBSCRIBED)           -> subscribed;
message_name(?UNSUBSCRIBE)          -> unsubscribe;
message_name(?UNSUBSCRIBED)         -> unsubscribed;
message_name(?EVENT)                -> event;
message_name(?EVENT_RECEIVED)       -> event_received;
message_name(?SUBSCRIBER_RECEIVED)  -> subscriber_received;
message_name(?CALL)                 -> call;
message_name(?CANCEL)               -> cancel;
message_name(?RESULT)               -> result;
message_name(?REGISTER)             -> register;
message_name(?REGISTERED)           -> registered;
message_name(?UNREGISTER)           -> unregister;
message_name(?UNREGISTERED)         -> unregistered;
message_name(?INVOCATION)           -> invocation;
message_name(?INTERRUPT)            -> interrupt;
message_name(?YIELD)                -> yield.