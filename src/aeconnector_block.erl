-module(aeconnector_block).

-export([height/1]).
-export([hash/1]).
-export([prev_hash/1]).
-export([txs/1]).

-export([block/4]).

-type tx() ::aeconnector_tx:tx().

-record(block, { hash::binary(), prev_hash::binary(), height::binary(), txs::list(tx()) }).

-type block() :: #block{}.

-export_type([block/0]).

-spec height(block()) -> non_neg_integer().
height(Block) ->
  Block#block.height.

-spec hash(block()) -> binary().
hash(Block) ->
  Block#block.hash.

-spec prev_hash(block()) -> binary().
prev_hash(Block) ->
  Block#block.prev_hash.

-spec txs(block()) -> list(tx()).
txs(Block) ->
  Block#block.txs.

-spec block(Height::non_neg_integer(), Hash::binary(), PrevHash::binary(), Txs::[tx()]) -> block().
block(Height, Hash, PrevHash, Txs) when
  is_integer(Height), is_binary(Hash), is_binary(PrevHash), is_list(Txs) ->
  #block{ height = Height, hash = Hash, prev_hash = PrevHash, txs = Txs}.
