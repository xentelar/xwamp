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

%% WARNING: DO NOT CHANGE THE ORDER OF THE RECORD FIELDS as they map
%% to the order in WAMP messages and we use list_to_tuple/1 to convert from
%% WAMP list to Erlang tuple and vicecersa

%% =============================================================================
%% COMMON
%% =============================================================================

-define(WAMP_CODECS, [
	json,
  msgpack,
  bert,
  cbor,
  erl,
  json_batched,
  msgpack_batched,
  bert_batched,
  erl_batched
	cbor_batched
]).

-define(MAX_ID, 9007199254740992).

-type codec()    ::  json
                  | msgpack
                  | bert
                  | cbor
                  | erl
                  | json_batched
                  | msgpack_batched
                  | bert_batched
                  | erl_batched
                  | cbor_batched.

-type uri()       ::  binary().
-type id()        ::  0..?MAX_ID.

%% =============================================================================
%% 2.3.1. WebSocket Transport
%% =============================================================================

-define(WAMP2_JSON, 						<<"wamp.2.json">>).
-define(WAMP2_MSGPACK, 			  	<<"wamp.2.msgpack">>).
-define(WAMP2_BERT, 						<<"wamp.2.bert">>).
-define(WAMP2_CBOR, 						<<"wamp.2.cbor">>).
-define(WAMP2_ERL, 						  <<"wamp.2.erl">>).
-define(WAMP2_JSON_BATCHED,			<<"wamp.2.json.batched">>).
-define(WAMP2_MSGPACK_BATCHED,	<<"wamp.2.msgpack.batched">>).
-define(WAMP2_BERT_BATCHED,			<<"wamp.2.bert.batched">>).
-define(WAMP2_CBOR_BATCHED,			<<"wamp.2.cbor.batched">>).
-define(WAMP2_ERL_BATCHED,			<<"wamp.2.erl.batched">>).

%% =============================================================================
%% 15.1. RawSocket Transport
%% =============================================================================

-define(HEAD, 16#7F).

% Handshake
% 
% MSB                                 LSB
% 31                                    0
% 0111 1111 LLLL SSSS RRRR RRRR RRRR RRRR
% 
% The possible values for LENGTH are:
%   0: 2**9 octets
%   1: 2**10 octets
%   ...
%   14: 2**23 octets
%   15: 2**24 octets
% 
% The possible values for SERIALIZER are:
%   0: illegal
%   1: JSON
%   2: MessagePack
%   3: CBOR
%   4: UBJSON
%   5: FlatBuffers
%   6 - 15: reserved for future serializers
% 
-define(HANDSHAKE(L, S),
  <<?HEAD:8, L:4, S:4, 0:16>>
).

% Handshake Error
% 
% MSB                                 LSB
% 31                                    0
% 0111 1111 EEEE 0000 RRRR RRRR RRRR RRRR
% 
% The possible values for ERROR are:
%   0: illegal (must not be used)
%   1: serializer unsupported
%   2: maximum message length unacceptable
%   3: use of reserved bits (unsupported feature)
%   4: maximum connection count reached
%   5 - 15: reserved for future errors
% 
-define(HANDSHAKE_ERROR(E),
  <<?HEAD:8, E:4, 0:20>>
).

% Framing
% 
% The serialized octets for a message to be sent are prefixed with exactly 4 octets.
% MSB                                 LSB
% 31                                    0
% RRRR XTTT LLLL LLLL LLLL LLLL LLLL LLLL
% The three least significant octets constitute an unsigned 24 bit integer that 
% provides the length of transport message payload following, excluding 
% the 4 octets that constitute the prefix.
% 
% The three bits TTT encode the type of the transport message:
%   0: regular WAMP message
%   1: PING
%   2: PONG
%   3-7: reserved

%   0: regular WAMP message
-define(RAW_MSG_PREFIX, <<0:4, 0:1, 0:3>>).

%   Regular frame WAMP message
-define(RAW_FRAME(BinMsg),
  <<(?RAW_MSG_PREFIX)/binary, (byte_size(BinMsg)):24, BinMsg/binary>>
).

%   1: PING
-define(RAW_PING_PREFIX, <<0:4, 0:1, 1:3>>).

%   2: PONG
-define(RAW_PONG_PREFIX, <<0:4, 0:1, 2:3>>).

%% =============================================================================
%% FEATURE ANNOUNCEMENT
%% =============================================================================

-define(ROUTER_ROLES_SPEC, #{
  <<"broker">> => #{
    alias => <<"broker">>,
    required => false,
    datatype => map,
    validator => #{
      features => #{
        alias => <<"features">>,
        required => false,
        datatype => map,
        validator => ?BROKER_FEATURES_SPEC
      }
    }
  },
  <<"dealer">> => #{
    alias => <<"dealer">>,
    required => false,
    datatype => map,
    validator => #{
      features => #{
        alias => <<"features">>,
        required => false,
        datatype => map,
        validator => ?DEALER_FEATURES_SPEC
      }
    }
  }
}).

-define(WAMP_COMMON_FEATURES_SPEC, #{
  <<"payload_passthru_mode">> => #{
    alias => <<"payload_passthru_mode">>,
    required => false,
    datatype => boolean
  }
}).

-define(WAMP_RPC_FEATURES_SPEC, (?WAMP_COMMON_FEATURES_SPEC)#{
  <<"progressive_call_results">> => #{
    alias => <<"progressive_call_results">>,
    required => false,
    datatype => boolean
  },
  <<"progressive_calls">> => #{
    alias => <<"progressive_calls">>,
    required => false,
    datatype => boolean
  },
  <<"call_timeout">> => #{
    alias => <<"call_timeout">>,
    required => false,
    datatype => boolean
  },
  <<"call_canceling">> => #{
    alias => <<"call_canceling">>,
    required => false,
    datatype => boolean
  },
  <<"caller_identification">>=> #{
    alias => <<"caller_identification">>,
    required => false,
    datatype => boolean
  },
  <<"sharded_registration">> => #{
    alias => <<"sharded_registration">>,
    required => false,
    datatype => boolean
  }
}).

-define(WAMP_DEALER_FEATURES_SPEC, (?WAMP_RPC_FEATURES_SPEC)#{
  <<"call_reroute">> => #{
    alias => <<"call_reroute">>,
    required => false,
    datatype => boolean
  },
  <<"call_trustlevels">> => #{
    alias => <<"call_trustlevels">>,
    required => false,
    datatype => boolean
  },
  <<"pattern_based_registration">> => #{
    alias => <<"pattern_based_registration">>,
    required => false,
    datatype => boolean
  },
  <<"shared_registration">> => #{
    alias => <<"shared_registration">>,
    required => false,
    datatype => boolean
  },
  <<"registration_revocation">> => #{
    alias => <<"registration_revocation">>,
    required => false,
    datatype => boolean
  },
  <<"testament_meta_api">> => #{
    alias => <<"testament_meta_api">>,
    required => false,
    datatype => boolean
  },
  <<"session_meta_api">> => #{
    alias => <<"session_meta_api">>,
    required => false,
    datatype => boolean
  },
  <<"registration_meta_api">> => #{
    alias => <<"registration_meta_api">>,
    required => false,
    datatype => boolean
  },
  <<"reflection">> => #{
    alias => <<"reflection">>,
    required => false,
    datatype => boolean
  }
}).

-define(DEALER_FEATURES_SPEC, ?WAMP_DEALER_FEATURES_SPEC#{
  <<"call_retries">> => #{
    alias => <<"call_retries">>,
    required => false,
    datatype => boolean
  }
}).

-define(WAMP_PUBSUB_FEATURES_SPEC, (?WAMP_COMMON_FEATURES_SPEC)#{
  <<"publisher_identification">> => #{
    alias => <<"publisher_identification">>,
    required => false,
    datatype => boolean
  },
  sharded_subscription => #{
    alias => <<"sharded_subscription">>,
    required => false,
    datatype => boolean
  }
}).

-define(BROKER_FEATURES_SPEC, (?WAMP_PUBSUB_FEATURES_SPEC)#{
  <<"subscriber_blackwhite_listing">> => #{
    alias => <<"subscriber_blackwhite_listing">>,
    required => false,
    datatype => boolean
  },
  <<"publisher_exclusion">> => #{
    alias => <<"publisher_exclusion">>,
    required => false,
    datatype => boolean
  },
  <<"publication_trustlevels">> => #{
    alias => <<"publication_trustlevels">>,
    required => false,
    datatype => boolean
  },
  <<"pattern_based_subscription">> => #{
    alias => <<"pattern_based_subscription">>,
    required => false,
    datatype => boolean
  },
  <<"event_history">> => #{
    alias => <<"event_history">>,
    required => false,
    datatype => boolean
  },
  <<"event_retention">> => #{
    alias => <<"event_retention">>,
    required => false,
    datatype => boolean
  },
  <<"subscription_revocation">> => #{
    alias => <<"subscription_revocation">>,
    required => false,
    datatype => boolean
  },
  <<"session_meta_api">> => #{
    alias => <<"session_meta_api">>,
    required => false,
    datatype => boolean
  },
  <<"subscription_meta_api">> => #{
    alias => <<"subscription_meta_api">>,
    required => false,
    datatype => boolean
  },
  <<"reflection">> => #{
    alias => <<"reflection">>,
    required => false,
    datatype => boolean
  },
  <<"acknowledge_subscriber_received">> => #{
    alias => <<"acknowledge_subscriber_received">>,
    required => false,
    datatype => boolean
  }
}).

-define(CLIENT_ROLES_SPEC, #{
  <<"publisher">> => #{
    alias => <<"publisher">>,
    required => false,
    datatype => map,
    validator => #{
      features => #{
        alias => <<"features">>,
        required => false,
        datatype => map,
        validator => ?PUBLISHER_FEATURES_SPEC
      }
    }
  },
  <<"subscriber">> => #{
    alias => <<"subscriber">>,
    required => false,
    datatype => map,
    validator => #{
      features => #{
        alias => <<"features">>,
        required => false,
        datatype => map,
        validator => ?SUBSCRIBER_FEATURES_SPEC
      }
    }
  },
  <<"caller">> => #{
    alias => <<"caller">>,
    required => false,
    datatype => map,
    validator => #{
      features => #{
        alias => <<"features">>,
        required => false,
        datatype => map,
        validator => ?CALLER_FEATURES_SPEC
      }
    }
  },
  <<"callee">> => #{
    alias => <<"callee">>,
    required => false,
    datatype => map,
    validator => #{
      features => #{
        alias => <<"features">>,
        required => false,
        datatype => map,
        validator => ?CALLEE_FEATURES_SPEC
      }
    }
  }
}).

-define(CALLEE_FEATURES_SPEC, (?WAMP_RPC_FEATURES_SPEC)#{
  <<"call_reroute">> => #{
    alias => <<"call_reroute">>,
    required => false,
    datatype => boolean
  },
  <<"call_trustlevels">> => #{
    alias => <<"call_trustlevels">>,
    required => false,
    datatype => boolean
  },
  <<"pattern_based_registration">> => #{
    alias => <<"pattern_based_registration">>,
    required => false,
    datatype => boolean
  },
  <<"shared_registration">> => #{
    alias => <<"shared_registration">>,
    required => false,
    datatype => boolean
  },
  <<"registration_revocation">> => #{
    alias => <<"registration_revocation">>,
    required => false,
    datatype => boolean
  },
  <<"testament_meta_api">> => #{
    alias => <<"testament_meta_api">>,
    required => false,
    datatype => boolean
  },
  <<"session_meta_api">> => #{
    alias => <<"session_meta_api">>,
    required => false,
    datatype => boolean
  },
  <<"registration_meta_api">> => #{
    alias => <<"registration_meta_api">>,
    required => false,
    datatype => boolean
  },
  <<"reflection">> => #{
    alias => <<"reflection">>,
    required => false,
    datatype => boolean
  }
}).

-define(CALLER_FEATURES_SPEC, ?WAMP_CALLER_FEATURES_SPEC#{
  <<"call_retries">> => #{
    alias => <<"call_retries">>,
    required => false,
    datatype => boolean
  }
}).

-define(WAMP_CALLER_FEATURES_SPEC, (?WAMP_RPC_FEATURES_SPEC)#{
}).

-define(SUBSCRIBER_FEATURES_SPEC, (?WAMP_PUBSUB_FEATURES_SPEC)#{
  <<"publication_trustlevels">> => #{
    alias => <<"publication_trustlevels">>,
    required => false,
    datatype => boolean
  },
  <<"pattern_based_subscription">> => #{
    alias => <<"pattern_based_subscription">>,
    required => false,
    datatype => boolean
  },
  <<"event_history">> => #{
    alias => <<"event_history">>,
    required => false,
    datatype => boolean
  },
  <<"subscription_revocation">> => #{
    alias => <<"subscription_revocation">>,
    required => false,
    datatype => boolean
  }
}).

-define(PUBLISHER_FEATURES_SPEC, (?WAMP_PUBSUB_FEATURES_SPEC)#{
  <<"subscriber_blackwhite_listing">> => #{
    alias => <<"subscriber_blackwhite_listing">>,
    required => false,
    datatype => boolean
  },
  <<"publisher_exclusion">> => #{
    alias => <<"publisher_exclusion">>,
    required => false,
    datatype => boolean
  }
}).

%% =============================================================================
%% WAMP MESSAGES
%% =============================================================================

-define(HELLO,    				    1).
-define(WELCOME,    			    2).
-define(ABORT,    				    3).
-define(CHALLENGE,  			    4).
-define(AUTHENTICATE,			    5).
-define(GOODBYE,    			    6).
-define(ERROR,    				    8).

%% -----------------------------------------------------------------------------
%% PUSUB
%% -----------------------------------------------------------------------------
-define(PUBLISH, 							16).
-define(PUBLISHED, 						17).
-define(SUBSCRIBE, 						32).
-define(SUBSCRIBED, 					33).
-define(UNSUBSCRIBE, 					34).
-define(UNSUBSCRIBED, 				35).
-define(EVENT, 								36).
-define(EVENT_RECEIVED,				37).
-define(SUBSCRIBER_RECEIVED,	38).

%% -----------------------------------------------------------------------------
%% RPC
%% -----------------------------------------------------------------------------
-define(CALL, 								48).
-define(CANCEL, 							49).
-define(RESULT, 							50).
-define(REGISTER, 						64).
-define(REGISTERED, 					65).
-define(UNREGISTER, 					66).
-define(UNREGISTERED,					67).
-define(INVOCATION, 					68).
-define(INTERRUPT, 						69).
-define(YIELD, 								70).

-type message_name()  ::  hello
                        | welcome
                        | abort
                        | challenge
                        | authenticate
                        | goodbye
                        | error
                        | publish
                        | published
                        | subscribe
                        | subscribed
                        | unsubscribe
                        | unsubscribed
                        | event
                        | call
                        | cancel
                        | result
                        | register
                        | registered
                        | unregister
                        | unregistered
                        | invocation
                        | interrupt
                        | yield.

-type wamp_message()   ::  wamp_hello()
                          | wamp_challenge()
                          | wamp_authenticate()
                          | wamp_welcome()
                          | wamp_abort()
                          | wamp_goodbye()
                          | wamp_error()
                          | wamp_publish()
                          | wamp_published()
                          | wamp_subscribe()
                          | wamp_subscribed()
                          | wamp_unsubscribe()
                          | wamp_unsubscribed()
                          | wamp_event()
                          | wamp_call()
                          | wamp_cancel()
                          | wamp_result()
                          | wamp_register()
                          | wamp_registered()
                          | wamp_unregister()
                          | wamp_unregistered()
                          | wamp_invocation()
                          | wamp_interrupt()
                          | wamp_yield().


-define(EXACT_MATCH,		<<"exact">>).
-define(PREFIX_MATCH, 	<<"prefix">>).
-define(WILDCARD_MATCH,	<<"wildcard">>).
-define(MATCH_STRATEGIES, [
		?EXACT_MATCH,
  ?PREFIX_MATCH,
  ?WILDCARD_MATCH
]).

-define(PPT_DETAILS_SPEC, #{
  %% The ppt_scheme identifies the Key Management Schema. It is a required
  %% string attribute. This attribute can contain the name or identifier of a
  %% key management provider that is known to the target peer, so it can be
  %% used to obtain information about encryption keys. A Router can recognize
  %% that Payload Passthru Mode is in use by checking the existence and
  %% non-empty value of this attribute within the options of CALL, PUBLISH
  %% and YIELD messages.
  <<"ppt_scheme">> => #{
    alias => <<"ppt_scheme">>,
    required => false,
    datatype => binary
  },
  %% The ppt_serializer attribute is optional. It specifies what serializer
  %% was used to encode the payload. It can be a value a such as mqtt, amqp,
  %% stomp to indicate that the incoming data is tunneling through such
  %% technologies, or it can be ordinary json, msgpack, cbor, flatbuffers
  %% data serializers.
  <<"ppt_serializer">> => #{
    alias => <<"ppt_serializer">>,
    required => false,
    datatype => binary
  },
  %% The ppt_cipher attribute is optional. It is required if the payload is
  %% encrypted. This attribute specifies the cryptographic algorithm that was
  %% used to encrypt the payload. It can be xsalsa20poly1305, aes256gcm for
  %% now.
  <<"ppt_cipher">> => #{
    alias => <<"ppt_cipher">>,
    required => false,
    datatype => binary
  },
  %% The ppt_keyid attribute is optional. This attribute can contain the
  %% encryption key id that was used to encrypt the payload. The ppt_keyid
  %% attribute is a string type. The value can be a hex-encoded string, URI,
  %% DNS name, Ethereum address, UUID identifier - any meaningful value which
  %% allows the target peer to choose a private key without guessing. The
  %% format of the value may depend on the ppt_scheme attribute.
  <<"ppt_keyid">> => #{
    alias => <<"ppt_keyid">>,
    required => false,
    datatype => binary
  }
}).

%% -----------------------------------------------------------------------------
%% HELLO 1
%% -----------------------------------------------------------------------------

-record(hello, {
  realm_uri   ::  uri(),
  details     ::  map()
}).

-type wamp_hello()  ::  #hello{}.

-define(HELLO_DETAILS_SPEC, #{
  <<"authmethods">> => #{
    alias => <<"authmethods">>,
    required => false,
    datatype => {list, binary}
  },
  <<"authid">> => #{
    alias => <<"authid">>,
    required => false,
    datatype => [binary, atom]
  },
  <<"authrole">> => #{
    alias => <<"authrole">>,
    required => false,
    datatype => [binary, atom]
  },
  <<"authextra">> => #{
    alias => <<"authextra">>,
    required => false,
    allow_undefined => true,
    allow_null => false,
    datatype => map
  },
  <<"roles">> => #{
    alias => <<"roles">>,
    required => true,
    datatype => map,
    validator => ?CLIENT_ROLES_SPEC
  },
  <<"agent">> => #{
    alias => <<"agent">>,
    required => false,
    datatype => binary
  },
  <<"transport">> => #{
    alias => <<"transport">>,
    required => false,
    datatype => map,
    validator => #{
      auth => #{required => true}
    }
  },
  <<"resumable">> => #{
    alias => <<"resumable">>,
    required => false,
    datatype => boolean
  },
  <<"resume_session">> => #{
    alias => <<"resume_session">>,
    required => false,
    datatype => binary
  },
  <<"resume_token">> => #{
    alias => <<"resume_token">>,
    required => false,
    datatype => binary
  }
}).

%% -----------------------------------------------------------------------------
%% WELCOME 2
%% -----------------------------------------------------------------------------

-record(welcome, {
  session_id  ::  id(),
  details     ::  map()
}).

-type wamp_welcome()     ::  #welcome{}.

-define(WELCOME_DETAILS_SPEC, #{
  <<"realm">> => #{
    alias => <<"realm">>,
    required => true,
    datatype => binary
  },
  <<"roles">> => #{
    alias => <<"roles">>,
    required => true,
    datatype => map,
    validator => ?ROUTER_ROLES_SPEC
  },
  <<"authid">> => #{
    alias => <<"authid">>,
    required => true,
    datatype => binary
  },
  <<"authrole">> => #{
    alias => <<"authrole">>,
    required => true,
    datatype => [binary, atom]
  },
  <<"authmethod">> => #{
    alias => <<"authmethod">>,
    required => false,
    datatype => binary
  },
  <<"authprovider">> => #{
    alias => <<"authprovider">>,
    required => false,
    datatype => binary
  },
  <<"authextra">> => #{
    alias => <<"authextra">>,
    required => false,
    allow_undefined => true,
    allow_null => false,
    datatype => map
  },
  <<"agent">> => #{
    alias => <<"agent">>,
    required => false,
    datatype => binary
  },
  <<"resumed">> => #{
    alias => <<"resumed">>,
    required => false,
    datatype => boolean
  },
  <<"resumable">> => #{
    alias => <<"resumable">>,
    required => false,
    datatype => boolean
  },
  <<"resume_token">> => #{
    alias => <<"resume_token">>,
    required => false,
    datatype => binary
  }
}).

%% -----------------------------------------------------------------------------
%% ABORT 3
%% -----------------------------------------------------------------------------

-record(abort, {
  details       ::  map(),
  reason_uri    ::  uri()
}).

-type wamp_abort()     ::  #abort{}.

-define(ABORT_DETAILS_SPEC, #{
  <<"message">> => #{
    alias => <<"message">>,
    required => false,
    datatype => binary
  }
}).

%% -----------------------------------------------------------------------------
%% CHALLENGE 4
%% -----------------------------------------------------------------------------

-record(challenge, {
  auth_method ::  binary(),
  extra       ::  map()
}).

-type wamp_challenge()     ::  #challenge{}.

-define(CHALLENGE_EXTRA_SPEC, #{
  <<"challenge">> => #{
    alias => <<"challenge">>,
    required => false,
    datatype => binary
  },
  %% For WAMP-CRA
  <<"keylen">> => #{
    alias => <<"keylen">>,
    required => false,
    datatype => integer
  },
  %% For WAMP-CRA & WAMP-SCRAM
  <<"salt">> => #{
    alias => <<"salt">>,
    required => false,
    datatype => binary
  },
  %% For WAMP-CRA & WAMP-SCRAM
  <<"iterations">> => #{
    alias => <<"iterations">>,
    required => false,
    datatype => integer
  },
  %% For WAMP-SCRAM
  <<"nonce">> => #{
    alias => <<"nonce">>,
    required => false,
    datatype => binary
  },
  %% For WAMP-SCRAM
  <<"memory">> => #{
    alias => <<"memory">>,
    required => false,
    allow_undefined => true,
    allow_null => false,
    datatype => integer
  }
}).

%% -----------------------------------------------------------------------------
%% AUTHENTICATE 5
%% -----------------------------------------------------------------------------

-record(authenticate, {
  signature   ::  binary(),
  extra       ::  map()
}).

-type wamp_authenticate()     ::  #authenticate{}.

%% -----------------------------------------------------------------------------
%% GOODBYE 6
%% -----------------------------------------------------------------------------

-record(goodbye, {
  details     ::  map(),
  reason_uri  ::  uri()
}).

-type wamp_goodbye()     ::  #goodbye{}.

-define(GOODBYE_DETAILS_SPEC, #{
  <<"message">> => #{
    alias => <<"message">>,
    required => false,
    datatype => binary
  }
}).

%% -----------------------------------------------------------------------------
%% ERROR 8
%% -----------------------------------------------------------------------------

-record(error, {
  request_type  ::  pos_integer(),
  request_id    ::  id(),
  details     ::  map(),
  error_uri     ::  uri(),
  args      ::  list() | undefined,
  kwargs      ::  map() | undefined
}).
-type wamp_error()     ::  #error{}.

-define(ERROR_DETAILS_SPEC, #{}).

%% -----------------------------------------------------------------------------
%% PUBLISH 16
%%
%% -----------------------------------------------------------------------------

-record(publish, {
  request_id    ::  id(),
  options       ::  map(),
  topic_uri     ::  uri(),
  args          ::  list() | undefined,
  kwargs        ::  map() | undefined
}).

-type wamp_publish()     ::  #publish{}.

-define(PUBLISH_OPTS_SPEC, ?PPT_DETAILS_SPEC#{
  %% resource key
  <<"acknowledge">> => #{
    alias => <<"acknowledge">>,
    required => false,
    datatype => boolean
  },
  <<"rkey">> => #{
    alias => <<"rkey">>,
    required => false,
    datatype => binary
  },
  <<"disclose_me">> => #{
    alias => <<"disclose_me">>,
    required => false,
    datatype => boolean
  },
  %% blacklisting
  <<"exclude">> => #{
    alias => <<"exclude">>,
    required => false,
    datatype => {list, integer}
  },
  <<"exclude_authid">> => #{
    alias => <<"exclude_authid">>,
    required => false,
    datatype => {list, binary}
  },
  <<"exclude_authrole">> => #{
    alias => <<"exclude_authrole">>,
    required => false,
    datatype => {list, binary}
  },
  <<"exclude_me">> => #{
    alias => <<"exclude_me">>,
    required => false,
    datatype => boolean
  },
  %% whitelisting
  <<"eligible">> => #{
    alias => <<"eligible">>,
    required => false,
    datatype => {list, integer}
  },
  <<"eligible_authid">> => #{
    alias => <<"eligible_authid">>,
    required => false,
    datatype => {list, binary}
  },
  <<"eligible_authrole">> => #{
    alias => <<"eligible_authrole">>,
    required => false,
    datatype => {list, binary}
  },
  <<"retain">> => #{
    alias => <<"retain">>,
    required => false,
    datatype => boolean
  }
}).

%% -----------------------------------------------------------------------------
%% PUBLISHED 17
%% -----------------------------------------------------------------------------

-record(published, {
  request_id      ::  id(),
  publication_id  ::  id()
}).

-type wamp_published()     ::  #published{}.

%% -----------------------------------------------------------------------------
%% SUBSCRIBE 32
%% -----------------------------------------------------------------------------

-record(subscribe, {
  request_id    ::  id(),
  options       ::  map(),
  topic_uri     ::  uri()
}).

-type wamp_subscribe()     ::  #subscribe{}.

-define(SUBSCRIBE_OPTS_SPEC, #{
  <<"match">> => #{
    alias => <<"match">>,
    required => true,
    default => ?EXACT_MATCH,
    datatype => {in, ?MATCH_STRATEGIES}
  },
  <<"nkey">> => #{
    alias => <<"nkey">>,
    required => false,
    datatype => binary
  },
  <<"get_retained">> => #{
    alias => <<"get_retained">>,
    required => false,
    datatype => boolean
  }
}).

%% -----------------------------------------------------------------------------
%% SUBSCRIBED 33
%% -----------------------------------------------------------------------------

-record(subscribed, {
  request_id      ::  id(),
  subscription_id ::  id()
}).
-type wamp_subscribed()     ::  #subscribed{}.

%% -----------------------------------------------------------------------------
%% UNSUBSCRIBE 34
%% -----------------------------------------------------------------------------

-record(unsubscribe, {
  request_id      ::  id(),
  subscription_id ::  id()
}).

-type wamp_unsubscribe()     ::  #unsubscribe{}.

%% -----------------------------------------------------------------------------
%% UNSUBSCRIBED 35
%% -----------------------------------------------------------------------------

-record(unsubscribed, {
  request_id    ::  id()
}).

-type wamp_unsubscribed()     ::  #unsubscribed{}.

%% -----------------------------------------------------------------------------
%% EVENT 36
%% -----------------------------------------------------------------------------

-record(event, {
  subscription_id ::  id(),
  publication_id  ::  id(),
  details         ::  map(),
  args            ::  list() | undefined,
  kwargs          ::  map() | undefined
}).

-type wamp_event()     ::  #event{}.

-define(EVENT_DETAILS_SPEC, ?PPT_DETAILS_SPEC#{
  <<"acknowledge">> => #{
    alias => <<"acknowledge">>,
    required => false,
    datatype => boolean
  },
  <<"topic">> => #{
    alias => <<"topic">>,
    required => false,
    datatype => binary
  },
  <<"publisher">> => #{
    alias => <<"publisher">>,
    required => false,
    datatype => integer
  },
  <<"publisher_authid">> => #{
    alias => <<"publisher_authid">>,
    required => false,
    datatype => binary
  },
  <<"publisher_authrole">> => #{
    alias => <<"publisher_authrole">>,
    required => false,
    datatype => binary
  },
  <<"retained">> => #{
    alias => <<"retained">>,
    required => false,
    datatype => boolean
  }
}).

%% -----------------------------------------------------------------------------
%% CALL 48
%% -----------------------------------------------------------------------------

-record(call, {
  request_id      ::  id(),
  options         ::  map(),
  procedure_uri   ::  uri(),
  args            ::  list() | undefined,
  kwargs          ::  map() | undefined
}).

-type wamp_call()     ::  #call{}.

-define(WAMP_CALL_OPTS_SPEC, ?PPT_DETAILS_SPEC#{
  <<"timeout">> => #{
    alias => <<"timeout">>,
    required => false,
    default => 0,
    datatype => non_neg_integer
  },
  <<"receive_progress">> => #{
    alias => <<"receive_progress">>,
    required => false,
    datatype => boolean
  },
  <<"disclose_me">> => #{
    alias => <<"disclose_me">>,
    required => false,
    datatype => boolean
  },
  <<"runmode">> => #{
    alias => <<"runmode">>,
    required => false,
    datatype => {in, [<<"partition">>]}
  },
  <<"rkey">> => #{
    alias => <<"rkey">>,
    required => false,
    datatype => binary
  }
}).

%% -----------------------------------------------------------------------------
%% CANCEL 49
%% -----------------------------------------------------------------------------

-record(cancel, {
  request_id    ::  id(),
  options       ::  map()
}).

-type wamp_cancel()     ::  #cancel{}.

-define(CALL_CANCELLING_OPTS_SPEC, #{
  <<"mode">> => #{
    alias => <<"mode">>,
    required => false,
    datatype => {in, [<<"skip">>, <<"kill">>, <<"killnowait">>]}
  }
}).

%% -----------------------------------------------------------------------------
%% RESULT 50
%% -----------------------------------------------------------------------------

-record(result, {
  request_id    ::  id(),
  details       ::  map(),
  args          ::  list() | undefined,
  kwargs        ::  map() | undefined
}).

-type wamp_result()     ::  #result{}.

-define(RESULT_DETAILS_SPEC, ?PPT_DETAILS_SPEC#{
  <<"progress">> => #{
    alias => <<"progress">>,
    required => false,
    datatype => boolean
  }
}).

%% -----------------------------------------------------------------------------
%% REGISTER 64
%% -----------------------------------------------------------------------------

-record(register, {
  request_id      ::  id(),
  options         ::  map(),
  procedure_uri   ::  uri()
}).

-type wamp_register()     ::  #register{}.

-define(INVOKE_SINGLE,      <<"single">>).
-define(INVOKE_ROUND_ROBIN, <<"roundrobin">>).
-define(INVOKE_RANDOM,      <<"random">>).
-define(INVOKE_FIRST,       <<"first">>).
-define(INVOKE_LAST,        <<"last">>).

-define(REGISTER_OPTS_SPEC, #{
  <<"disclose_caller">> => #{
    alias => <<"disclose_caller">>,
    required => false,
    datatype => boolean
  },
  <<"match">> => #{
    alias => <<"match">>,
    required => false,
    default => ?EXACT_MATCH,
    datatype => {in, ?MATCH_STRATEGIES}
  },
  <<"invoke">> => #{
    alias => <<"invoke">>,
    required => false,
    default => ?INVOKE_SINGLE,
    datatype => {in, [
      ?INVOKE_SINGLE,
      ?INVOKE_ROUND_ROBIN,
      ?INVOKE_RANDOM,
      ?INVOKE_FIRST,
      ?INVOKE_LAST
    ]}
  }
}).

%% -----------------------------------------------------------------------------
%% REGISTERED 65
%% -----------------------------------------------------------------------------

-record(registered, {
  request_id      ::  id(),
  registration_id ::  id()
}).

-type wamp_registered()     ::  #registered{}.

%% -----------------------------------------------------------------------------
%% UNREGISTER 66
%% -----------------------------------------------------------------------------

-record(unregister, {
  request_id      ::  id(),
  registration_id ::  id()
}).

-type wamp_unregister()     ::  #unregister{}.

%% -----------------------------------------------------------------------------
%% UNREGISTERED 67
%% -----------------------------------------------------------------------------

-record(unregistered, {
  request_id  ::  id(),
  details     ::  map() | undefined
}).

-type wamp_unregistered()   ::  #unregistered{}.

-define(UNREGISTERED_DETAILS_SPEC, #{
  <<"reason">> => #{
    alias => <<"reason">>,
    required => false,
    datatype => binary
  },
  <<"registration">> => #{
    alias => <<"registration">>,
    required => false,
    datatype => integer
  }
}).

%% -----------------------------------------------------------------------------
%% INVOCATION 68
%% -----------------------------------------------------------------------------

-record(invocation, {
  request_id      ::  id(),
  registration_id ::  id(),
  details         ::  map(),
  args            ::  list() | undefined,
  kwargs          ::  map() | undefined
}).

-type wamp_invocation()     ::  #invocation{}.

-define(INVOCATION_DETAILS_SPEC, ?PPT_DETAILS_SPEC#{
  <<"trustlevel">> => #{
    alias => <<"trustlevel">>,
    required => false,
    datatype => integer
  },
  <<"procedure">> => #{
    alias => <<"procedure">>,
    required => false,
    datatype => binary
  },
  <<"receive_progress">> => #{
    alias => <<"receive_progress">>,
    required => false,
    datatype => boolean
  },
  <<"caller">> => #{
    alias => <<"caller">>,
    required => false,
    datatype => integer
  },
  <<"caller_authid">> => #{
    alias => <<"caller_authid">>,
    required => false,
    datatype => binary
  },
  <<"caller_authrole">> => #{
    alias => <<"caller_authrole">>,
    required => false,
    datatype => binary
  }
}).

%% -----------------------------------------------------------------------------
%% INTERRUPT 69
%% -----------------------------------------------------------------------------

-record(interrupt, {
  request_id    ::  id(),
  options       ::  map()
}).

-type wamp_interrupt()     ::  #interrupt{}.


-define(INTERRUPT_OPTIONS_SPEC, #{
  <<"mode">> => #{
    alias => <<"mode">>,
    required => false,
    datatype => binary
  }
}).

%% -----------------------------------------------------------------------------
%% YIELD 70
%% -----------------------------------------------------------------------------

-record(yield, {
  request_id    ::  id(),
  options       ::  map(),
  args          ::  list() | undefined,
  kwargs        ::  map() | undefined
}).

-type wamp_yield()  ::  #yield{}.

-define(YIELD_OPTIONS_SPEC, ?PPT_DETAILS_SPEC).

%% =============================================================================
%% URIs
%% =============================================================================

%% -----------------------------------------------------------------------------
%% 8. Basic Profile URIs
%% -----------------------------------------------------------------------------

-define(WAMP_ERROR_INVALID_URI,     						      <<"wamp.error.invalid_uri">>).
-define(WAMP_ERROR_NO_SUCH_PROCEDURE, 				        <<"wamp.error.no_such_procedure">>).
-define(WAMP_ERROR_PROCEDURE_ALREADY_EXISTS,		      <<"wamp.error.procedure_already_exists">>).
-define(WAMP_ERROR_NO_SUCH_REGISTRATION, 				      <<"wamp.error.no_such_registration">>).
-define(WAMP_ERROR_NO_SUCH_SUBSCRIPTION, 				      <<"wamp.error.no_such_subscription">>).
-define(WAMP_ERROR_INVALID_ARGUMENT, 						      <<"wamp.error.invalid_argument">>).
-define(WAMP_ERROR_CANCELLED, 									      <<"wamp.error.canceled">>).
-define(WAMP_ERROR_PAYLOAD_SIZE_EXCEEDED,				      <<"wamp.error.payload_size_exceeded">>).
-define(WAMP_ERROR_SYSTEM_SHUTDOWN,       			      <<"wamp.close.system_shutdown">>).
-define(WAMP_ERROR_CLOSE_REALM, 								      <<"wamp.close.close_realm">>).
-define(WAMP_ERROR_GOODBYE_AND_OUT, 						      <<"wamp.close.goodbye_and_out">>).
-define(WAMP_ERROR_PROTOCOL_VIOLATION,  				      <<"wamp.error.protocol_violation">>).

%% -----------------------------------------------------------------------------
%% 18. Advanced Profile URIs
%% -----------------------------------------------------------------------------

% 18.1. Session Close
-define(WAMP_ERROR_CLOSE_KILLED, 								      <<"wamp.close.killed">>).
-define(WAMP_ERROR_NO_SUCH_SESSION,   					      <<"wamp.error.no_such_session">>).

% 18.2. Authentication
-define(WAMP_ERROR_NO_MATCHING_AUTH_METHOD, 	        <<"wamp.error.no_matching_auth_method">>).
-define(WAMP_ERROR_NO_SUCH_REALM, 							      <<"wamp.error.no_such_realm">>).
-define(WAMP_ERROR_NO_SUCH_ROLE, 								      <<"wamp.error.no_such_role">>).
-define(WAMP_ERROR_NO_SUCH_PRINCIPAL, 					      <<"wamp.error.no_such_principal">>).
-define(WAMP_ERROR_AUTHENTICATION_DENIED, 			      <<"wamp.error.authentication_denied">>).
-define(WAMP_ERROR_AUTHENTICATION_FAILED, 			      <<"wamp.error.authentication_failed">>).
-define(WAMP_ERROR_AUTHENTICATION_REQUIRED, 		      <<"wamp.error.authentication_required">>).

% 18.3. Authorization
-define(WAMP_ERROR_NOT_AUTHORIZED, 							      <<"wamp.error.not_authorized">>).
-define(WAMP_ERROR_AUTHORIZATION_DENIED, 				      <<"wamp.error.authorization_denied">>).
-define(WAMP_ERROR_AUTHORIZATION_FAILED, 				      <<"wamp.error.authorization_failed">>).
-define(WAMP_ERROR_AUTHORIZATION_REQUIRED, 			      <<"wamp.error.authorization_required">>).

% 18.4. Remote Procedure Calls
-define(WAMP_ERROR_TIMEOUT,														<<"wamp.error.timeout">>).
-define(WAMP_ERROR_OPTION_NOT_ALLOWED,								<<"wamp.error.option_not_allowed">>).
-define(WAMP_ERROR_OPTION_DISALLOWED_DISCLOSE_ME,			<<"wamp.error.option_disallowed.disclose_me">>).
-define(WAMP_ERROR_NET_FAILURE, 											<<"wamp.error.network_failure">>).
-define(WAMP_ERROR_UNAVAILABLE, 											<<"wamp.error.unavailable">>).
-define(WAMP_ERROR_NO_AVAILABLE_CALLEE, 							<<"wamp.error.no_available_callee">>).
-define(WAMP_ERROR_FEATURE_NOT_SUPPORTED, 						<<"wamp.error.feature_not_supported">>).

%% -----------------------------------------------------------------------------
%% 10.2. Registration Meta API
%% -----------------------------------------------------------------------------

% 10.2.1. Events
-define(WAMP_REGISTRATION_ON_CREATE,  				        <<"wamp.registration.on_create">>).
-define(WAMP_REGISTRATION_ON_DELETE,  				        <<"wamp.registration.on_delete">>).
-define(WAMP_REGISTRATION_ON_REGISTER,  			        <<"wamp.registration.on_register">>).
-define(WAMP_REGISTRATION_ON_UNREGISTER, 			        <<"wamp.registration.on_unregister">>).

% 10.2.2. Procedures
-define(WAMP_COUNT_CALLEES,       							      <<"wamp.registration.count_callees">>).
-define(WAMP_LIST_CALLEES,        							      <<"wamp.registration.list_callees">>).
-define(WAMP_REG_GET,           								      <<"wamp.registration.get">>).
-define(WAMP_REG_LIST,          								      <<"wamp.registration.list">>).
-define(WAMP_REG_LOOKUP,        								      <<"wamp.registration.lookup">>).
-define(WAMP_REG_MATCH,         								      <<"wamp.registration.match">>).

%% -----------------------------------------------------------------------------
%% 10.3. Subscriptions Meta API
%% -----------------------------------------------------------------------------

% 10.3.1. Events
-define(WAMP_SUBSCRIPTION_ON_CREATE,      		        <<"wamp.subscription.on_create">>).
-define(WAMP_SUBSCRIPTION_ON_DELETE,      		        <<"wamp.subscription.on_delete">>).
-define(WAMP_SUBSCRIPTION_ON_SUBSCRIBE,     	        <<"wamp.subscription.on_subscribe">>).
-define(WAMP_SUBSCRIPTION_ON_UNSUBSCRIBE,             <<"wamp.subscription.on_unsubscribe">>).

% 10.3.2. Procedures
-define(WAMP_SUBSCRIPTION_COUNT_SUBSCRIBERS,		      <<"wamp.subscription.count_subscribers">>).
-define(WAMP_SUBSCRIPTION_GET,          				      <<"wamp.subscription.get">>).
-define(WAMP_SUBSCRIPTION_LIST,         				      <<"wamp.subscription.list">>).
-define(WAMP_SUBSCRIPTION_LIST_SUBSCRIBERS,			      <<"wamp.subscription.list_subscribers">>).
-define(WAMP_SUBSCRIPTION_LOOKUP,         			      <<"wamp.subscription.lookup">>).
-define(WAMP_SUBSCRIPTION_MATCH,        				      <<"wamp.subscription.match">>).