%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
-module(aeconnector).

-export([connect/3]).
-export([get_block_by_hash/2, get_top_block/1]).
-export([dry_send_tx/3, send_tx/3]).
-export([disconnect/1]).

-type connector() :: atom().

-type block() :: aeconnector_block:block().

-callback connect(map(), function()) -> {ok, pid()} | {error, term()}.

-callback dry_send_tx(binary(), binary()) -> boolean().
-callback send_tx(binary(), binary()) -> ok.

-callback get_top_block() -> {ok, binary()} | {error, term()}.
-callback get_block_by_hash(binary()) -> {ok, block()} | {error, term()}.

-callback disconnect() -> ok.

-export_type([connector/0]).

%%%===================================================================
%%%  API
%%%===================================================================
-spec connect(connector(), map(), function()) -> {ok, pid()} | {error, term()}.
connect(Con, Args, Callback) ->
  try
    Res = {ok, Pid} = Con:connect(Args, Callback), true = is_pid(Pid),
    Res
  catch E:R ->
    {error, E, R}
  end.

-spec dry_send_tx(connector(), binary(), binary()) -> boolean().
dry_send_tx(Con, Account, Payload) ->
  try
    Res = Con:dry_send_tx(Account, Payload), true = is_boolean(Res),
    Res
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

-spec get_top_block(connector()) -> {ok, binary()} | {error, term()}.
get_top_block(Con) ->
  try
    Res = {ok, Hash} = Con:get_top_block(), true = is_binary(Hash),
    Res
  catch E:R ->
    {error, {E, R}}
  end.

-spec get_block_by_hash(connector(), binary()) -> {ok, block()} | {error, term()}.
get_block_by_hash(Con, Hash) ->
  try
    Res = {ok, Block} = Con:get_block_by_hash(Hash), true = aeconnector_block:is_block(Block),
    [true = aeconnector_tx:is_tx(Tx)|| Tx <- aeconnector_block:txs(Block)],
    Res
  catch E:R ->
    {error, {E, R}}
  end.

-spec disconnect(connector()) -> ok.
disconnect(Con) ->
  ok = Con:disconnect().