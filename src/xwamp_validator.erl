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
%% @doc xwamp_validator public API
%% @end
%% -----------------------------------------------------------------------------
-module(xwamp_validator).

-include("xwamp.hrl").

-export([validate_payload/3]).
-export([is_valid_id/1]).
-export([validate_id/1]).

-export([validate_map/3]).
-export([validate_map/4]).

%% =============================================================================
%% API
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec is_valid_id(id()) -> boolean().
is_valid_id(N) when is_integer(N) andalso N >= 0 andalso N =< ?MAX_ID ->
  true;
is_valid_id(_) ->
  false.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
validate_id(Id) ->
  is_valid_id(Id) == true orelse error({invalid_id, Id}),
  Id.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec validate_map(map(), term(), term()) -> term().
validate_map(Map, Spec, Extensions) ->
  Opts = #{
    atomic => true, % Fail atomically for the whole map
    labels => binary,  % This will only turn the defined keys to atoms
    keep_unknown => false % Remove all unknown options
  },
  validate_map(Map, Spec, Extensions, Opts).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec validate_map(map(), term(), term(), map()) -> term().
validate_map(Map, Spec, Extensions, Opts0) ->
  Defaults = #{
    atomic => true, % Fail atomically for the whole map
    labels => binary,  % This will only turn the defined keys to atoms
    keep_unknown => false % Remove all unknown options
  },
  Opts = maps:merge(Defaults, Opts0),
  NewSpec = maybe_add_extensions(Extensions, Spec),

  try
    maps_utils:validate(Map, NewSpec, Opts)
  catch
    error:Reason when is_map(Reason) ->
      error({validation_failed, Reason})
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec validate_payload(list() | undefined, map() | undefined, map()) -> tuple().
validate_payload([], undefined, _) ->
  {undefined, undefined};

validate_payload([], KWArgs, _) ->
  {undefined, validate_kwargs(KWArgs)};

validate_payload(Args, KWArgs, _) ->
  {Args, validate_kwargs(KWArgs)}.

%% =============================================================================
%% PRIVATE
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec maybe_add_extensions(list(), term()) -> list() | term().
maybe_add_extensions(Keys, Spec) when is_list(Keys) ->
  MaybeAdd = fun(Key, Acc) -> maybe_add_extension(Key, Acc) end,
  lists:foldl(MaybeAdd, Spec, Keys);

maybe_add_extensions([], Spec) ->
  Spec.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec maybe_add_extension(tuple() | binary() | atom(), map()) -> map().
maybe_add_extension({invoke, Options}, Acc) ->
  #{datatype := {in, L}} = KeySpec = maps:get(invoke, Acc),
  NewKeySpec = KeySpec#{
    datatype => {in, lists:append(L, Options)}
  },
  maps:put(invoke, NewKeySpec, Acc);

maybe_add_extension(Key, Acc) ->
  try
    %% We add a maps_utils key specification for the known
    NewKey = to_valid_extension_key(Key),
    NewKeySpec = #{
      %% we alias it so that maps_utils:validate/2 can find the key
      %% and replace it with NewKey.
      alias => atom_to_binary(Key, utf8),
      required => false
    },
    maps:put(NewKey, NewKeySpec, Acc)
  catch
    throw:invalid_key ->
      Acc
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec to_valid_extension_key(atom() | binary()) -> atom().
to_valid_extension_key(Key) when is_atom(Key) ->
  to_valid_extension_key(atom_to_binary(Key, utf8));

to_valid_extension_key(Key) when is_binary(Key) ->
  try
    {match, _} = re:run(Key, extension_key_pattern()),
    %% We add a maps_utils key specification for the known
    binary_to_existing_atom(Key, utf8)
  catch
    error:{badmatch, nomatch} ->
      throw(invalid_key);
    error:badarg ->
      throw(invalid_key)
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec extension_key_pattern() -> term().
extension_key_pattern() ->
  CompiledPattern = persistent_term:get({?MODULE, ekey_pattern}, undefined),
  extension_key_pattern(CompiledPattern).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec extension_key_pattern(term()) -> term().
extension_key_pattern(undefined) ->
  {ok, Pattern} = re:compile("_[a-z0-9_]{3,}"),
  ok = persistent_term:put({?MODULE, ekey_pattern}, Pattern),
  Pattern;

extension_key_pattern(CompiledPattern) ->
  CompiledPattern.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec validate_kwargs(term()) -> term().
validate_kwargs(undefined) ->
  undefined;

validate_kwargs(KWArgs) when is_map(KWArgs) andalso map_size(KWArgs) =:= 0 ->
  undefined;

validate_kwargs(KWArgs) when is_map(KWArgs) ->
  KWArgs.