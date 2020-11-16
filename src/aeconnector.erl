%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
-module(aeconnector).

-export([dry_send_tx/3, send_tx/3, is_signed/3, get_block_by_hash/2, get_top_block/1]).
-export([tx/2, block/4]).

-type connector() :: atom().

-record(tx, { account::binary(), payload::binary() }).
-record(block, { hash::binary(), prev_hash::binary(), height::binary(), txs::list(tx()) }).

-type tx() :: #tx{}.
-type block() :: #block{}.

-callback connect(map(), function()) -> {ok, pid()} | {error, term()}.

-callback is_signed(binary(), binary()) -> boolean().

-callback send_tx(binary(), binary()) -> ok.
-callback get_top_block() -> block().
-callback get_block_by_hash(binary()) -> block().

-callback dry_send_tx(binary(), binary()) -> ok.

-callback disconnect() -> ok.

-export_type([connector/0]).

%%%===================================================================
%%%  API
%%%===================================================================

-spec dry_send_tx(connector(), binary(), binary()) -> ok | {error, term()}.
dry_send_tx(Con, Account, Payload) ->
  try
    ok = Con:dry_send_tx(Account, Payload)
  catch E:R ->
    {error, {E, R}}
  end.

-spec send_tx(connector(), binary(), binary()) -> ok | {error, term()}.
send_tx(Con, Account, Payload) ->
  try
    ok = Con:send_tx(Account, Payload)
  catch E:R ->
    {error, {E, R}}
  end.

-spec is_signed(connector(), binary(), binary()) -> boolean().
is_signed(Con, Account, Payload) ->
  try
    ok = Con:is_signed(Account, Payload)
  catch E:R ->
    {error, {E, R}}
  end.

-spec get_top_block(connector()) -> {ok, block()} | {error, term()}.
get_top_block(Con) ->
  try
    Res = Con:get_top_block(),
    {ok, Res}
  catch E:R ->
    {error, {E, R}}
  end.

-spec get_block_by_hash(connector(), binary()) -> {ok, block()} | {error, term()}.
get_block_by_hash(Con, Hash) ->
  try
    Res = Con:get_block_by_hash(Hash),
    {ok, Res}
  catch E:R ->
    {error, {E, R}}
  end.

%%%===================================================================
%%%  Protocol
%%%===================================================================

-spec tx(Account::binary(), Payload::binary()) -> tx().
tx(Account, Payload)
  when is_binary(Account), is_binary(Payload) ->
  #tx{ account = Account, payload = Payload}.

-spec block(Height::non_neg_integer(), Hash::binary(), PrevHash::binary(), Txs::[tx()]) -> block().
block(Height, Hash, PrevHash, Txs) when
  is_integer(Height), is_binary(Hash), is_binary(PrevHash), is_list(Txs) ->
  #block{ height = Height, hash = Hash, prev_hash = PrevHash, txs = Txs}.
