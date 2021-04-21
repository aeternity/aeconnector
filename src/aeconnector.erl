%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
-module(aeconnector).

-export([connect/3]).
-export([get_block_by_hash/2, get_top_block/1]).
-export([dry_send_tx/2, send_tx/2]).
-export([push_tx/2, pop_tx/1]).
-export([disconnect/1]).

-export([priv_dir/0]).

-export([amount/1]).
-export([to_hex/1, from_hex/1]).

-type connector() :: atom().

-type hash() :: binary().
-type payload() :: binary().

-type block() :: aeconnector_block:block().

-type item() :: aeconnector_schedule:item().

-callback connect(map(), function()) -> {ok, pid()} | {error, term()}.

-callback dry_send_tx(payload()) -> boolean().
-callback send_tx(payload()) -> ok | {error, term()}.

-callback get_top_block() -> {ok, hash()} | {error, term()}.
-callback get_block_by_hash(hash()) -> {ok, block()} | {error, term()}.

-callback push_tx(item()) -> ok.
-callback pop_tx() -> {ok, item()} | {error, term()}.

%% Report should be implemented as a part of behaviour
-callback disconnect() -> ok.

-optional_callbacks([push_tx/1, pop_tx/0]).

-export_type([connector/0]).

-spec priv_dir() -> file:filename().
priv_dir() ->
  code:priv_dir(?MODULE).

-spec amount(float()) -> binary().
amount(Amount) ->
  float_to_binary(Amount, [{decimals, 4}]).

-spec to_hex(binary()) -> binary().
to_hex(Payload) ->
  ToHex = fun (X) -> integer_to_binary(X,16) end,
  _HexData = << <<(ToHex(X))/binary>> || <<X:4>> <= Payload >>.

-spec from_hex(binary()) -> binary().
from_hex(HexData) ->
  ToInt = fun (H, L) -> binary_to_integer(<<H, L>>,16) end,
  _Payload = << <<(ToInt(H, L))>> || <<H:8, L:8>> <= HexData >>.

%%%===================================================================
%%%  Connector API
%%%===================================================================
-spec connect(connector(), map(), function()) -> {ok, pid()} | {error, term()}.
connect(Con, Args, Callback) ->
  try
    Res = {ok, Pid} = Con:connect(Args, Callback), true = is_pid(Pid),
    Res
  catch E:R ->
    {error, E, R}
  end.

-spec dry_send_tx(connector(), payload()) -> boolean().
dry_send_tx(Con, Payload) ->
  try
    Res = Con:dry_send_tx(Payload), true = is_boolean(Res),
    Res
  catch E:R ->
    {error, {E, R}}
  end.

-spec send_tx(connector(), payload()) -> ok | {error, term()}.
send_tx(Con, Payload) ->
  try
    ok = Con:send_tx(Payload)
  catch E:R ->
    {error, {E, R}}
  end.

-spec get_top_block(connector()) -> {ok, hash()} | {error, term()}.
get_top_block(Con) ->
  try
    Res = {ok, Hash} = Con:get_top_block(), true = is_binary(Hash),
    Res
  catch E:R ->
    {error, {E, R}}
  end.

-spec get_block_by_hash(connector(), hash()) -> {ok, block()} | {error, term()}.
get_block_by_hash(Con, Hash) ->
  try
    Res = {ok, Block} = Con:get_block_by_hash(Hash), true = aeconnector_block:is_block(Block),
    [true = aeconnector_tx:is_tx(Tx)|| Tx <- aeconnector_block:txs(Block)],
    Res
  catch E:R ->
    {error, {E, R}}
  end.

-spec push_tx(connector(), item()) -> ok | {error, term()}.
push_tx(Con, Item) ->
  try
      ok = Con:push_tx(Item)
  catch
      E:R ->
        {error, {E, R}}
  end.

-spec pop_tx(connector()) -> {ok, item()} | {error, term()}.
pop_tx(Con) ->
  try
    Res = {ok, Item} = Con:pop_tx(), true = aeconnector_schedule:is_item(Item),
    Res
  catch E:R ->
    {error, {E, R}}
  end.

-spec disconnect(connector()) -> ok.
disconnect(Con) ->
  ok = Con:disconnect().
