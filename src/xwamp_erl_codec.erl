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
%% @doc xwamp_erl_codec public API
%% @end
%% -----------------------------------------------------------------------------
-module(xwamp_erl_codec).

-record(wamp_erl, {
  keys = binary             ::  binary
                                | atom | existing_atom | attempt_atom,
  tuple_as_list = true      ::  boolean(),
  message_format = list     ::  tuple | list,
  level = 0                 ::  non_neg_integer()
}).

-type encode_opts() ::  [encode_opt()].
-type encode_opt()  ::  {keys, binary | atom | existing_atom | attempt_atom}.
-type decode_opts() ::  [].


-export([encode/1]).
-export([encode/2]).

-export([decode/1]).
-export([decode/2]).


%% =============================================================================
%% API
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec encode(term()) -> term().
encode(Term) ->
  encode(Term, []).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec encode(term(), term()) -> term().
encode(Term, Opts) ->
  encode_term(Term, parse_options(Opts)).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec decode(binary()) -> term().
decode(Bin) ->
  decode(Bin, []).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec decode(_, _) -> _.
decode(Bin, Opts) ->
  decode_term(Bin, parse_options(Opts)).

%% =============================================================================
%% PRIVATE
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec encode_term(term(), encode_opts()) -> term().
encode_term(Term, #wamp_erl{level = Level}) 
              when is_atom(Term), Term =/= undefined, not is_boolean(Term), Level > 1 ->
  %% Level 1 atoms are the keys of the WAMP messages so we key them
  %% Level 2+ are keys of the payload and details. It is dangerous for
  atom_to_binary(Term);

encode_term(Term, Opts0) when is_map(Term) ->
  Opts = incr(Opts0),
  maps:fold(
    fun(K, V, Acc) ->
      maps:put(
        encode_term(K, Opts),
        encode_term(V, Opts),
        Acc
      )
    end,
    #{},
    Term
  );

encode_term([{_, _}|_] = Term, Opts0) ->
  Opts = incr(Opts0),

  maps:from_list(
    lists:map(
      fun({K, V}) ->
        {encode_term(K, Opts), encode_term(V, Opts)} end,
      Term
    )
  );

encode_term(Term, Opts0) when is_list(Term) ->
  Opts = incr(Opts0),
  % TODO: Handle improper lists
  lists:map(fun(T) -> encode_term(T, Opts) end, Term);

encode_term(Term, #wamp_erl{level = 0} = Opts) when is_tuple(Term) ->
  Encoded = encode_term(tuple_to_list(Term), Opts),
  case Opts#wamp_erl.message_format of
    tuple ->
      list_to_tuple(Encoded);
    list ->
      Encoded
  end;

encode_term(Term, Opts0) when is_tuple(Term) ->
  Encoded = encode_term(tuple_to_list(Term), incr(Opts0)),

  case Opts0#wamp_erl.tuple_as_list of
    true ->
      Encoded;
    false ->
      list_to_tuple(Encoded)
  end;

encode_term(Term, _) when is_pid(Term) ->
  list_to_binary(pid_to_list(Term));

encode_term(Term, _) when is_port(Term) ->
  list_to_binary(port_to_list(Term));

encode_term(Term, _) ->
  Term.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec decode_term(term(), decode_opts()) -> term().
decode_term(<<"true">>, _) ->
  %% Just in case
  true;

decode_term(<<"false">>, _) ->
  %% Just in case
  false;

decode_term(<<"null">>, _) ->
  undefined;

decode_term(Term, Opts0) when is_map(Term) ->
  Opts = incr(Opts0),

  maps:fold(
    fun(K, V, Acc) ->
      maps:put(
        maybe_atom(decode_term(K, Opts), Opts),
        decode_term(V, Opts),
        Acc
      )
    end,
    #{},
    Term
  );

decode_term(Term, Opts) when is_list(Term) ->
  lists:map(fun(T) -> decode_term(T, incr(Opts)) end, Term);

decode_term(Term, #wamp_erl{level = 0} = Opts) when is_tuple(Term) ->
  list_to_tuple(
    decode_term(tuple_to_list(Term), incr(Opts))
  );

decode_term(Term, _) when is_tuple(Term) ->
  error({badarg, Term});

decode_term(Term, _) ->
  Term.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec maybe_atom(term(), tuple()) -> term().
maybe_atom(Term, #wamp_erl{keys = binary}) ->
  Term;

maybe_atom(Term, #wamp_erl{keys = atom}) ->
  binary_to_atom(Term);

maybe_atom(Term, #wamp_erl{keys = existing_atom}) ->
  binary_to_existing_atom(Term, utf8);

maybe_atom(Term, #wamp_erl{keys = attempt_atom}) ->
  try
    binary_to_existing_atom(Term, utf8)
  catch
    error:badarg ->
    Term
  end.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec parse_options(term()) -> term().
parse_options(Options) ->
  parse_option(Options, #wamp_erl{}).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec parse_option(term(), term()) -> term().
parse_option([], Opt) ->
  Opt;

parse_option([{keys, K}|T], Opt) 
          when K =:= binary; K =:= atom; K =:= existing_atom; K =:= attempt_atom ->
  parse_option(T, Opt#wamp_erl{keys = K});

parse_option(List, Opt) ->
  error(badarg, [List, Opt]).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec incr(tuple()) -> tuple().
incr(#wamp_erl{level = Level} = Opts) ->
  Opts#wamp_erl{level = Level + 1}.