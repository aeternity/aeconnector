%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2021, Aeternity Anstalt
%%% @doc
%%% @end
-module(aeconnector_offline_SUITE).

%% API
-export([suite/0]).

-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([groups/0]).

-export([all/0]).

-export([connect/1]).
-export([dry_send_tx/1, send_tx/1]).
-export([get_top_block/1]).
-export([get_block_by_hash/1]).
-export([synchronize/1]).
-export([disconnect/1]).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
  [].

init_per_suite(Config) ->
  ok = lager:start(),
  ok = application:start(yamerl),
  Payload = <<"kh_Z6MucDvCV1HbgEN91QY6Hn6exY58v7oKp9uk2jatLVVusLdWL">>,
  Pointer = <<"0000000000000001d8d2952308de99c312382fb78ab5a3edb2cd17ce59987cc5">>,
  Setup =
    [
      {payload, Payload},
      {pointer, Pointer},
      {return_address, ?MODULE}
    ],
  lists:append(Setup, Config).

end_per_suite(_Config) ->
  ok.

init_per_group(_GroupName, Config) ->
  Config.

end_per_group(_GroupName, _Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

groups() ->
  [].

all() ->
  [connect, dry_send_tx, send_tx, get_top_block, synchronize, get_block_by_hash, disconnect].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

connect(Config) ->
  Priv = aeconnector:priv_dir(), Script = filename:join(Priv, "offline.yaml"),
  Args = #{
    <<"script">> => Script,
    <<"account">> => <<"0014c0be2b090aab44c79d0883c8f3bc5d32afbcc9a7">>
  },
  ReturnAddress = ?config(return_address, Config),
  Callback =
    fun (_Con, Block) ->
      ReturnAddress ! Block
    end,
  {ok, Pid} = aeconnector:connect(offline_conncetor(), Args, Callback),
  {comment, Pid}.

dry_send_tx(Config) ->
  Payload = ?config(payload, Config),
  true = aeconnector:dry_send_tx(offline_conncetor(), Payload),
  ok.

send_tx(_Config) ->
  Payload = <<"Hyperchains trace">>,
  [ok = aeconnector:send_tx(offline_conncetor(), Payload) || _ <- lists:seq(0, 10)],
  ok.

synchronize(Config) ->
  ReturnAddress = ?config(return_address, Config),
  erlang:register(ReturnAddress, self()),
  receive
    Block ->
      true = aeconnector_block:is_block(Block),
      {comment, Block}
  end.

get_top_block(_Config) ->
  {ok, Hash} = aeconnector:get_top_block(offline_conncetor()),
  true = is_binary(Hash),
  {comment, Hash}.

get_block_by_hash(Config) ->
  HexPointer = ?config(pointer, Config),

  Pointer = aeconnector:from_hex(HexPointer),
  {ok, Block} = aeconnector:get_block_by_hash(offline_conncetor(), Pointer),
  true = aeconnector_block:is_block(Block),
  {comment, Block}.

disconnect(_Config) ->
  ok = aeconnector:disconnect(offline_conncetor()).

offline_conncetor() ->
  aeconnector_offline.
