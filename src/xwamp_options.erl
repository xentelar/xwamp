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
%% @doc xwamp_options public API
%% @end
%% -----------------------------------------------------------------------------
-module(xwamp_options).

-include("xwamp.hrl").

-type type()  ::  publish
                | subscribe
                | call
                | cancel
                | register
                | interrupt
                | yield.

-export_type([type/0]).

-export([new/2]).
-export([options/1]).

%% =============================================================================
%% API
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc Fails with an exception if the Options maps is not valid.
%% A Options map is valid if all its properties (keys) are valid. A property is
%% valid if it is a key defined by the WAMP Specification for the message type
%% or when the key is found in the list of extended_options configured in the
%% application environment and in both cases the key is valid according to the
%% WAMP regex specification.
%%
%% Example:
%%
%% ```
%% application:set_env(wamp, extende_options, [{call, [<<"_x">>, <<"_y">>]}).
%% ```
%%
%% Using this configuration only `call' messages would accept `<<"_x">>'
%% and `<<"_y">>' properties.
%% -----------------------------------------------------------------------------
-spec new(MessageType :: type(), Opts :: map()) -> map() | no_return().
new(Type, Opts) ->
  Extensions = [],%xwamp_cfg:extended_options(Type),
  validate(Type, Opts, Extensions).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec options(wamp_call() | wamp_cancel() | wamp_interrupt()
            | wamp_publish() | wamp_register() | wamp_subscribe() 
            | wamp_yield()) -> map() | no_return().
options(#call{options = Val}) -> Val;
options(#cancel{options = Val}) -> Val;
options(#interrupt{options = Val}) -> Val;
options(#publish{options = Val}) -> Val;
options(#register{options = Val}) -> Val;
options(#subscribe{options = Val}) -> Val;
options(#yield{options = Val}) -> Val;
options(_) ->
  error(badarg).

%% =============================================================================
%% PRIVATE
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec validate(atom(), term(), term()) -> term().
validate(call, Opts, Extensions) ->
  Spec = ?WAMP_CALL_OPTS_SPEC,
  xwamp_validator:validate_map(Opts, Spec, Extensions);

validate(cancel, Opts, Extensions) ->
  Spec = ?CALL_CANCELLING_OPTS_SPEC,
  xwamp_validator:validate_map(Opts, Spec, Extensions);

validate(interrupt, Opts, Extensions) ->
  Spec = ?INTERRUPT_OPTIONS_SPEC,
  xwamp_validator:validate_map(Opts, Spec, Extensions);

validate(publish, Opts, Extensions) ->
  Spec = ?PUBLISH_OPTS_SPEC,
  xwamp_validator:validate_map(Opts, Spec, Extensions);

validate(register, Opts, Extensions) ->
  Spec = ?REGISTER_OPTS_SPEC,
  xwamp_validator:validate_map(Opts, Spec, Extensions);

validate(subscribe, Opts, Extensions) ->
  Spec = ?SUBSCRIBE_OPTS_SPEC,
  xwamp_validator:validate_map(Opts, Spec, Extensions);

validate(yield, Opts, Extensions) ->
  Spec = ?YIELD_OPTIONS_SPEC,
  xwamp_validator:validate_map(Opts, Spec, Extensions);

validate(_, _, _) ->
  error(badarg).