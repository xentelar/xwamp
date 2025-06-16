%% -----------------------------------------------------------------------------
%%
%% Copyright (c) 2025 Xentelar Advanced Technologies. All Rights Reserved.
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
%% @doc xwamp_message_builder public API
%% @end
%% -----------------------------------------------------------------------------
-module(xwamp_message_builder).

-include("xwamp.hrl").

-export([hello/2]).
-export([welcome/2]).
-export([goodbye/2]).

-export([challenge/2]).
-export([authenticate/2]).

-export([register/3]).
-export([registered/2]).
-export([unregister/2]).
-export([unregistered/1]).
-export([unregistered/2]).

-export([call/3]).
-export([call/4]).
-export([call/5]).

-export([invocation/3]).
-export([invocation/4]).
-export([invocation/5]).

-export([yield/2]).
-export([yield/3]).
-export([yield/4]).

-export([result/2]).
-export([result/3]).
-export([result/4]).

-export([subscribe/3]).
-export([subscribed/2]).
-export([unsubscribe/2]).
-export([unsubscribed/1]).

-export([publish/3]).
-export([publish/4]).
-export([publish/5]).
-export([published/2]).

-export([event/3]).
-export([event/4]).
-export([event/5]).

-export([error/4]).
-export([error/5]).
-export([error/6]).

-export([cancel/2]).
-export([interrupt/2]).
-export([abort/2]).


%% =============================================================================
%% API
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% If Details argument is not valid fails with an exception
%% @end
%% -----------------------------------------------------------------------------
-spec hello(uri(), map()) -> wamp_hello() | no_return().
hello(RealmUri, Details) when is_binary(RealmUri) ->
  #hello{
    realm_uri = xwamp_uri:validate(RealmUri),
    details = xwamp_details:new(hello, Details)
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec welcome(id(), map()) -> wamp_welcome() | no_return().
welcome(SessionId, Details)   ->
  #welcome{
    session_id = xwamp_validator:validate_id(SessionId),
    details = xwamp_details:new(welcome, Details)
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
%% "ABORT" gets sent only _before_ a _Session_ is established
-spec abort(map(), uri()) -> wamp_abort() | no_return().
abort(Details, ReasonUri) ->
  #abort{
    reason_uri = xwamp_uri:validate(ReasonUri),
    details = xwamp_details:new(abort, Details)
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec challenge(binary(), map()) -> wamp_challenge() | no_return().
challenge(AuthMethod, Extra) when is_map(Extra) ->
  #challenge{
    auth_method = AuthMethod,
    extra = Extra
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec authenticate(binary(), map()) -> wamp_authenticate() | no_return().
authenticate(Signature, Extra) when is_map(Extra) ->
  #authenticate{
    signature = Signature,
    extra = Extra
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
%% "GOODBYE" is sent only _after_ a _Session_ is already established.
-spec goodbye(map(), uri()) -> wamp_goodbye() | no_return().
goodbye(Details, ReasonUri) ->
  #goodbye{
    reason_uri = xwamp_uri:validate(ReasonUri),
    details = xwamp_details:new(goodbye, Details)
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec error(pos_integer(), id(), map(), uri()) -> wamp_error() | no_return().
error(ReqType, ReqId, Details, ErrorUri) ->
  error(ReqType, ReqId, Details, ErrorUri, undefined, undefined).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec error(pos_integer(), id(), map(), uri(), list()) -> wamp_error() | no_return().
error(ReqType, ReqId, Details, ErrorUri, Args) when is_list(Args) ->
  error(ReqType, ReqId, Details, ErrorUri, Args, undefined).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec error(pos_integer(), id(), map(), uri(), list() | undefined, map() | undefined) ->
                                      wamp_error() | no_return().
error(ReqType, ReqId, Details0, ErrorUri, Args0, KWArgs0) when is_map(Details0) ->
  {Args, KWArgs} = xwamp_validator:validate_payload(Args0, KWArgs0, Details0),

  #error{
    request_type = ReqType,
    request_id = xwamp_validator:validate_id(ReqId),
    details = Details0,
    error_uri = xwamp_uri:validate(ErrorUri),
    args = Args, kwargs = KWArgs
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec publish(id(), map(), uri()) -> wamp_publish() | no_return().
publish(ReqId, Options, TopicUri) ->
  publish(ReqId, Options, TopicUri, undefined, undefined).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec publish(id(), map(), uri(), list()) -> wamp_publish() | no_return().
publish(ReqId, Options, TopicUri, Args) when is_list(Args) ->
  publish(ReqId, Options, TopicUri, Args, undefined).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec publish(id(), map(), uri(), list() | undefined, map() | undefined) ->
  wamp_publish() | no_return().
publish(ReqId, Options0, TopicUri, Args0, KWArgs0) ->
  Options = xwamp_options:new(publish, Options0),
  {Args, KWArgs} = xwamp_validator:validate_payload(Args0, KWArgs0, Options),

  #publish{
    request_id = xwamp_validator:validate_id(ReqId),
    options = Options,
    topic_uri = xwamp_uri:validate(TopicUri),
    args = Args, kwargs = KWArgs
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec published(id(), id()) -> wamp_published() | no_return().
published(ReqId, PubId) ->
  #published{
    request_id = xwamp_validator:validate_id(ReqId),
    publication_id = xwamp_validator:validate_id(PubId)
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec subscribe(id(), map(), uri()) -> wamp_subscribe() | no_return().
subscribe(ReqId, Options0, TopicUri) when is_map(Options0) ->
  Options = xwamp_options:new(subscribe, Options0),
  Match = maps:get(match, Options, ?EXACT_MATCH),

  #subscribe{
    request_id = xwamp_validator:validate_id(ReqId),
    options = Options,
    topic_uri = xwamp_uri:validate(TopicUri, Match)
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec subscribed(id(), id()) -> wamp_subscribed() | no_return().
subscribed(ReqId, SubsId) ->
  #subscribed{
    request_id = xwamp_validator:validate_id(ReqId),
    subscription_id = xwamp_validator:validate_id(SubsId)
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec unsubscribe(id(), id()) -> wamp_unsubscribe() | no_return().
unsubscribe(ReqId, SubsId) ->
  #unsubscribe{
    request_id = xwamp_validator:validate_id(ReqId),
    subscription_id = xwamp_validator:validate_id(SubsId)
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec unsubscribed(id()) -> wamp_unsubscribed() | no_return().
unsubscribed(ReqId) ->
  #unsubscribed{
    request_id = xwamp_validator:validate_id(ReqId)
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec event(id(), id(), map()) -> wamp_event() | no_return().
event(SubsId, PubId, Details) ->
  event(SubsId, PubId, Details, undefined, undefined).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec event(id(), id(), map(), list()) -> wamp_event() | no_return().
event(SubsId, PubId, Details, Args) when is_list(Args) ->
  event(SubsId, PubId, Details, Args, undefined).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec event(id(), id(), map(), list() | undefined, map() | undefined) ->
  wamp_event() | no_return().
event(SubsId, PubId, Details0, Args0, KWArgs0) ->
  Details = xwamp_details:new(event, Details0),
  {Args, KWArgs} = xwamp_validator:validate_payload(Args0, KWArgs0, Details),

  #event{
    subscription_id = xwamp_validator:validate_id(SubsId),
    publication_id = xwamp_validator:validate_id(PubId),
    details = Details,
    args = Args,
    kwargs = KWArgs
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec call(id(), map(), uri()) -> wamp_call() | no_return().
call(ReqId, Options, ProcedureUri) ->
  call(ReqId, Options, ProcedureUri, undefined, undefined).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec call(id(), map(), uri(), list()) -> wamp_call() | no_return().
call(ReqId, Options, ProcedureUri, Args) when is_list(Args) ->
  call(ReqId, Options, ProcedureUri, Args, undefined).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec call(id(), map(), uri(), list() | undefined, map() | undefined) ->
                                  wamp_call() | no_return().
call(ReqId, Options0, ProcedureUri, Args0, KWArgs0) ->
  Options = xwamp_options:new(call, Options0),
  {Args, KWArgs} = xwamp_validator:validate_payload(Args0, KWArgs0, Options),

  #call{
    request_id = xwamp_validator:validate_id(ReqId),
    options = Options,
    procedure_uri = xwamp_uri:validate(ProcedureUri),
    args = Args,
    kwargs = KWArgs
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec cancel(id(), map()) -> wamp_cancel() | no_return().
cancel(ReqId, Options) ->
  #cancel{
    request_id = xwamp_validator:validate_id(ReqId),
    options = xwamp_options:new(cancel, Options)
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec result(id(), map()) -> wamp_result() | no_return().
result(ReqId, Details) ->
  result(ReqId, Details, undefined, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec result(id(), map(), list()) -> wamp_result() | no_return().
result(ReqId, Details, Args) when is_list(Args) ->
  result(ReqId, Details, Args, undefined).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec result(id(), map(), list() | undefined, map() | undefined) ->
                              wamp_result() | no_return().
result(ReqId, Details0, Args0, KWArgs0) when is_map(Details0) ->
  Details = xwamp_details:new(result, Details0),
  {Args, KWArgs} = xwamp_validator:validate_payload(Args0, KWArgs0, Details),

  #result{
    request_id = xwamp_validator:validate_id(ReqId),
    details = Details,
    args = Args,
    kwargs = KWArgs
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec register(id(), map(), uri()) -> wamp_register() | no_return().
register(ReqId0, Options0, ProcedureUri) ->
  ReqId = xwamp_validator:validate_id(ReqId0),
  Options = xwamp_options:new(register, Options0),
  Match = maps:get(match, Options, ?EXACT_MATCH),

  #register{
    request_id = ReqId,
    options = Options,
    procedure_uri = xwamp_uri:validate(ProcedureUri, Match)
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec registered(id(), id()) -> wamp_registered() | no_return().
registered(ReqId, RegId) ->
  #registered{
    request_id = xwamp_validator:validate_id(ReqId),
    registration_id = xwamp_validator:validate_id(RegId)
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec unregister(id(), id()) -> wamp_unregister() | no_return().
unregister(ReqId, RegId) ->
  #unregister{
    request_id = xwamp_validator:validate_id(ReqId),
    registration_id = xwamp_validator:validate_id(RegId)
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec unregistered(id()) -> wamp_unregistered() | no_return().
unregistered(ReqId) ->
  #unregistered{
    request_id = xwamp_validator:validate_id(ReqId)
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec unregistered(id(), map()) -> wamp_unregistered() | no_return().
unregistered(ReqId, Details) when is_map(Details) ->
  #unregistered{
    request_id = xwamp_validator:validate_id(ReqId),
    details = Details
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec invocation(id(), id(), map()) -> wamp_invocation() | no_return().
invocation(ReqId, RegId, Details) ->
  invocation(ReqId, RegId, Details, undefined, undefined).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec invocation(id(), id(), map(), list()) -> wamp_invocation() | no_return().
invocation(ReqId, RegId, Details, Args) when is_list(Args) ->
  invocation(ReqId, RegId, Details, Args, undefined).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec invocation(id(), id(), map(), list() | undefined, map() | undefined) ->
                                  wamp_invocation() | no_return().
invocation(ReqId, RegId, Details0, Args0, KWArgs0) ->
  Details = xwamp_details:new(invocation, Details0),
  {Args, KWArgs} = xwamp_validator:validate_payload(Args0, KWArgs0, Details),

  #invocation{
    request_id = xwamp_validator:validate_id(ReqId),
    registration_id = xwamp_validator:validate_id(RegId),
    details = Details,
    args = Args, kwargs = KWArgs
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec interrupt(id(), map()) -> wamp_interrupt() | no_return().
interrupt(ReqId, Options) ->
  #interrupt{
    request_id = xwamp_validator:validate_id(ReqId),
    options = xwamp_options:new(interrupt, Options)
  }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec yield(id(), map()) -> wamp_yield() | no_return().
yield(ReqId, Options) ->
  yield(ReqId, Options, undefined, undefined).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec yield(id(), map(), list()) -> wamp_yield() | no_return().
yield(ReqId, Options, Args) when is_list(Args) ->
  yield(ReqId, Options, Args, undefined).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec yield(id(), map(), list() | undefined, map() | undefined) ->
  wamp_yield() | no_return().
yield(ReqId, Options0, Args0, KWArgs0) ->
  Options = xwamp_options:new(yield, Options0),
  {Args, KWArgs} = xwamp_validator:validate_payload(Args0, KWArgs0, Options),

  #yield{
    request_id = xwamp_validator:validate_id(ReqId),
    options = Options,
    args = Args, kwargs = KWArgs
  }.
