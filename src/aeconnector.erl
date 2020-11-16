%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
-module(aeconnector).

-export([dry_send_tx/3, send_tx/3, is_signed/3, get_block_by_hash/2, get_top_block/1]).

-type connector() :: atom().

-type block() :: aeconnector_block:block().

-callback connect(map(), function()) -> {ok, pid()} | {error, term()}.

-callback is_signed(binary(), binary()) -> boolean().

-callback dry_send_tx(binary(), binary()) -> ok.
-callback send_tx(binary(), binary()) -> ok.

-callback get_top_block() -> {ok, binary()} | {error, term()}.
-callback get_block_by_hash(binary()) -> {ok, block()} | {error, term()}.

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

-spec get_top_block(connector()) -> {ok, binary()} | {error, term()}.
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

