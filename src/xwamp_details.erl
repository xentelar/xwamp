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
%% @doc xwamp_details public API
%% @end
%% -----------------------------------------------------------------------------
-module(xwamp_details).

-include("xwamp.hrl").

-type type()	::	hello
								| welcome
								| abort
								| goodbye
								| event
								| result
								| invocation
								| event_received
								| subscriber_received.


-export_type([type/0]).
-export([new/2]).

%% =============================================================================
%% API
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc 
%% Fails with an exception if the Options maps is not valid.
%% A Options map is valid if all its properties (keys) are valid. A property is
%% valid if it is a key defined by the WAMP Specification for the message type
%% or when the key is found in the list of extended_options configured in the
%% application environment and in both cases the key is valid according to the
%% WAMP regex specification.
%%
%% -----------------------------------------------------------------------------
-spec new(MessageType :: type(), Details :: map()) -> map() | no_return().

new(Type, Details) ->
  %Extensions = app_config:extended_details(Type),
  Extensions = [],
  validate(Type, Details, Extensions).

%% =============================================================================
%% PRIVATE
%% =============================================================================

%% @private
-spec validate(atom(), term(), term()) -> term().
validate(hello, Details0, Extensions) ->
  Spec = ?HELLO_DETAILS_SPEC,
  Details = xwamp_validator:validate_map(Details0, Spec, Extensions),
  Roles =  maps:get(<<"roles">>, Details),

  maps:size(Roles) > 0
    orelse error(#{
      code => missing_required_value,
      message => <<"No WAMP peer roles defined.">>,
      description => <<
        "At least one WAMP peer role is required in the "
        "HELLO.Details.roles dictionary"
      >>
    }),

  case key_value:get([<<"caller">>, <<"progressive_call_results">>], Details, false) of
    true ->
      key_value:get([<<"caller">>, <<"call_canceling">>], Details, false)
      orelse error(#{
        code => invalid_feature_request,
        message => <<"Invalid feature requested for Caller role">>,
        description => <<
          "The feature progressive_call_results was requested "
          "but the feature call_canceling was not, both need to be "
          "requested for progressive_call_results to be enabled."
        >>
      });
    false ->
      ok
  end,

  case key_value:get([<<"callee">>, <<"progressive_call_results">>], Details, false) of
    true ->
      key_value:get([<<"callee">>, <<"call_canceling">>], Details, false)
      orelse error(#{
        code => invalid_feature_request,
        message => <<"Invalid feature requested for Callee role">>,
        description => <<
          "The feature progressive_call_results was requested "
          "but the feature call_canceling was not, both need to be "
          "requested for progressive_call_results to be enabled."
        >>
      });
    false ->
      ok
  end,

  Details;

validate(welcome, Details, Extensions) ->
  Spec = ?WELCOME_DETAILS_SPEC,
  xwamp_validator:validate_map(Details, Spec, Extensions);

validate(abort, Details, Extensions) ->
  Spec = #{},
  Opts = #{keep_unknown => true},
  xwamp_validator:validate_map(Details, Spec, Extensions, Opts);

validate(goodbye, Details, Extensions) ->
  Spec = #{},
  Opts = #{keep_unknown => true},
  xwamp_validator:validate_map(Details, Spec, Extensions, Opts);

validate(error, Details, Extensions) ->
  Spec = ?ERROR_DETAILS_SPEC,
  xwamp_validator:validate_map(Details, Spec, Extensions);

validate(event, Details, Extensions) ->
  Spec = ?EVENT_DETAILS_SPEC,
  xwamp_validator:validate_map(Details, Spec, Extensions);

validate(result, Details, Extensions) ->
  Spec = ?RESULT_DETAILS_SPEC,
  xwamp_validator:validate_map(Details, Spec, Extensions);

validate(invocation, Details, Extensions) ->
  Spec = ?INVOCATION_DETAILS_SPEC,
  xwamp_validator:validate_map(Details, Spec, Extensions);

validate(_, _, _) ->
  error(badarg).