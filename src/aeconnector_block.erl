%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc

%%% @end
-module(aeconnector_block).

-export([height/1]).
-export([hash/0, hash/1]).
-export([prev_hash/1]).
-export([txs/1, txs/2]).

-export([block/4]).
-export([is_block/1]).

-type tx() ::aeconnector_tx:tx().

-record(block, { hash::binary(), prev_hash::binary() |  null, height::binary(), txs::[tx()] }).

-type block() :: #block{}.

-export_type([block/0]).

-spec hash() -> integer().
hash() ->
  #block.hash.

-spec height(block()) -> non_neg_integer().
height(Block) ->
  Block#block.height.

-spec hash(block()) -> binary().
hash(Block) ->
  Block#block.hash.

-spec prev_hash(block()) -> binary().
prev_hash(Block) ->
  Block#block.prev_hash.

-spec txs(block()) -> [tx()].
txs(Block) ->
  Block#block.txs.

-spec txs(block(), [tx()]) -> block().
txs(Block, Txs) ->
  Block#block{ txs = Txs }.

-spec block(Height::non_neg_integer(), Hash::binary(), PrevHash::binary(), Txs::[tx()]) -> block().
block(Height, Hash, PrevHash, Txs) when
  is_integer(Height), is_binary(Hash), is_binary(PrevHash), is_list(Txs) ->
  Block = #block{ height = Height, hash = Hash, prev_hash = PrevHash},
  txs(Block, Txs).

-spec is_block(term()) -> boolean().
is_block(#block{}) ->
  true;
is_block(_) ->
  false.