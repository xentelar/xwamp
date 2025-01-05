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
%% @doc xwamp_error_builder public API
%% @end
%% -----------------------------------------------------------------------------
-module(xwamp_error_builder).

-include("xwamp.hrl").

-type error_source()  ::  wamp_subscribe()
                          | wamp_unsubscribe()
                          | wamp_publish()
                          | wamp_register()
                          | wamp_unregister()
                          | wamp_call()
                          | wamp_invocation()
                          | wamp_cancel().

-export_type([error_source/0]).

-export([error_from/3]).
-export([error_from/4]).
-export([error_from/5]).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec error_from(error_source(), map(), uri()) -> wamp_error() | no_return().
error_from(M, Details, ErrorUri) ->
  error_from(M, Details, ErrorUri, undefined, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec error_from(error_source(), map(), 
                  uri(), list()) -> wamp_error() | no_return().
error_from(M, Details, ErrorUri, Args) ->
  error_from(M, Details, ErrorUri, Args, undefined).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec error_from(error_source(), map(), uri(), 
                list() | undefined, map() | undefined) -> wamp_error() | no_return().
error_from(#register{request_id = ReqId}, Details, ErrorUri, Args, KWArgs) ->
  xwamp_message_builder:error(?REGISTER, ReqId, Details, ErrorUri, Args, KWArgs);

error_from(#unregister{request_id = ReqId}, Details, ErrorUri, Args, KWArgs) ->
  xwamp_message_builder:error(?UNREGISTER, ReqId, Details, ErrorUri, Args, KWArgs);

error_from(#call{} = M, Details0, ErrorUri, Args, KWArgs) ->
  ReqId = M#call.request_id,
  Details = maybe_merge_details(M#call.options, Details0),
  xwamp_message_builder:error(?CALL, ReqId, Details, ErrorUri, Args, KWArgs);

error_from(#cancel{request_id = ReqId}, Details, ErrorUri, Args, KWArgs) ->
  xwamp_message_builder:error(?CANCEL, ReqId, Details, ErrorUri, Args, KWArgs);

error_from(#invocation{} = M, Details0, ErrorUri, Args, KWArgs) ->
  ReqId = M#invocation.request_id,
  Details = maybe_merge_details(M#invocation.details, Details0),
  xwamp_message_builder:error(?INVOCATION, ReqId, Details, ErrorUri, Args, KWArgs);

error_from(#subscribe{request_id = ReqId}, Details, ErrorUri, Args, KWArgs) ->
  xwamp_message_builder:error(?SUBSCRIBE, ReqId, Details, ErrorUri, Args, KWArgs);

error_from(#unsubscribe{request_id = ReqId}, Details, ErrorUri, Args, KWArgs) ->
  xwamp_message_builder:error(?UNSUBSCRIBE, ReqId, Details, ErrorUri, Args, KWArgs);

error_from(#publish{} = M, Details0, ErrorUri, Args, KWArgs) ->
  ReqId = M#publish.request_id,
  Details = maybe_merge_details(M#publish.options, Details0),
  xwamp_message_builder:error(?PUBLISH, ReqId, Details, ErrorUri, Args, KWArgs).

%% =============================================================================
%% PRIVATE
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec maybe_merge_details(map(), map()) -> map().
maybe_merge_details(MessageAttrs, Details) ->
  Attrs = [ppt_cipher, ppt_keyid, ppt_scheme, ppt_serializer],
  maps:merge(Details, maps:with(Attrs, MessageAttrs)).