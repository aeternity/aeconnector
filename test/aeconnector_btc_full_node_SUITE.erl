%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% This module executes BTC integration acceptance suite for the "user" and "delegate" roles.
%%% The process requires opened connection to the BTC network and described by the next steps:
%%% For the "user" role (default)
%%% 1. Open connection to the network (connect);
%%% 2. Get the hash of the current best block (get_top_block);
%%% 3. Fetch the chain from the current best block to the landed entry (pointer);
%%% 4. Synchronize the new mined block (a time depends on BTC network: ~8 min) (sync)
%%% 5. Close connection (disconnect);
%%% For the "delegate" role
%%% 1. Open connection to the network (connect);
%%% 2. Check available inputs, balance (dry_send_tx);
%%% 3. Put scheduled commitments into queue;
%%% 4. Pop scheduled commitment from a queue;
%%% 5. Send commitment transactions with predefined payload (send_tx);
%%% 6. Close connection (disconnect);
%%% @end
-module(aeconnector_btc_full_node_SUITE).

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
-export([fetch/1]).
-export([synchronize/1]).
-export([push_tx/1, pop_tx/1]).
-export([disconnect/1]).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
  [{timetrap,{minutes,60}}].

init_per_suite(Config) ->
  ok = lager:start(),
  %% TODO: This data should be externally configured!
  Payload = <<"kh_Z6MucDvCV1HbgEN91QY6Hn6exY58v7oKp9uk2jatLVVusLdWL">>,
  Pointer = <<"0000000046eac05f2f5bd64f29da6e8482c8928edd6edbf2ba4378f5f263a0a9">>,
  ReturnAddress = ?MODULE,
  Setup =
    [
      {payload, Payload},
      {pointer, Pointer},
      {return_address, ReturnAddress}
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

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% Description: Returns a list of test case group definitions.
%%--------------------------------------------------------------------
%% TODO: To make nested, conditional groups
groups() ->
  [
    {user, [sequence], [connect, get_top_block, get_block_by_hash, fetch, disconnect]}
%%    {delegate, [sequence], [connect, dry_send_tx, push_tx, pop_tx, send_tx, disconnect]}
  ].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%% Reason = term()
%%   The reason for skipping all groups and test cases.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%--------------------------------------------------------------------
all() ->
  [
    {group, user}%%, {group, delegate}
  ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% Description: Test case function. (The name of it must be specified in
%%              the all/0 list or in a test case group for the test case
%%              to be executed).
%%--------------------------------------------------------------------
%% TODO: To make Args configurable
connect(Config) ->
  Args = #{
    <<"user">> => <<"hyperchains">>,
    <<"password">> => <<"qwerty">>,
    <<"host">> => <<"127.0.0.1">>,
    <<"port">> => 8332,
    <<"ssl">> => false,
    <<"timeout">> => 30000,
    <<"webhook">> => <<"https://api.telegram.org/bot1615195542:AAEVQKT6I0yC3PVpmztjlejYd5ZM4KndPKA/sendMessage?chat_id=195084888&parse_mode=html">>,
    <<"address">> => <<"tb1qczlzkzg24dzv08ggs0y080zax2hmejd8k2x00l">>,
    <<"privatekey">> => <<"cQ2xqWRwtkhbN2MfCPtrMDwdpEnjvGgqfDhXPjLcaf8EqN5v2oRQ">>,
    <<"wallet">> => <<"Hyperchains">>,
    <<"amount">> => 0.0001,
    <<"fee">> => 0.00001
  },
  ReturnAddress = ?config(return_address, Config),
  Callback =
    fun (_Con, Block) ->
      ct:log(info, "~nThe new mined block is: ~p~n", [Block]),
      ReturnAddress ! Block
    end,
  {ok, Pid} = aeconnector:connect(btc_conncetor(), Args, Callback),
  {comment, Pid}.

get_top_block(_Config) ->
  {ok, Hash} = aeconnector:get_top_block(btc_conncetor()),
  true = is_binary(Hash),
  {comment, Hash}.

get_block_by_hash(Config) ->
  HexPointer = ?config(pointer, Config),

  Pointer = aeconnector:from_hex(HexPointer),
  {ok, Block} = aeconnector:get_block_by_hash(btc_conncetor(), Pointer),
  true = aeconnector_block:is_block(Block),
  {comment, Block}.

fetch(Config) -> ok.

%%fetch(Config) ->
%%  {ok, Top} = aeconnector:get_top_block(btc_conncetor()),
%%  Pointer = ?config(pointer, Config),
%%
%%  fun Fetch(Hash) ->
%%    {ok, Block} = aeconnector:get_block_by_hash(btc_conncetor(), Pointer),
%%    ct:log("~nThe fecthed block: ~p~n",[Block]),
%%    PrevHash = aeconnector_block:prev_hash(Block),
%%    (Hash == Pointer) orelse Fetch(PrevHash)
%%  end(Top),
%%  {comment, {Top, Pointer}}.

synchronize(Config) ->
  ReturnAddress = ?config(return_address, Config),
  erlang:register(ReturnAddress, self()),
  receive
    Block ->
      true = aeconnector_block:is_block(Block),
      {comment, Block}
  end.

dry_send_tx(Config) ->
  Payload = ?config(payload, Config),
  true = aeconnector:dry_send_tx(btc_conncetor(), Payload),
  ok.

push_tx(_Config) ->
  Radek = <<"tb1qczlzkzg24dzv08ggs0y080zax2hmejd8k2x00l">>,
  Grzegorz = <<"2NGZfw3NhM7NgRkY8RD8DoPkDDDK31QGVeh">>,
  Empty = <<"2NGZfw3NhM7NgRkY8RD8DoPkDDDK31QGVeh">>,
  Scheduled = [
    aeconnector_schedule:item(Radek, 0.001, 0.0009, <<"Few bucks to help survive in Munich">>),
    aeconnector_schedule:item(Grzegorz, 0.001, 0.0009, <<"Ticket to visit raccoons in a zoo">>),
    aeconnector_schedule:item(Empty, 0.001, 0.0009, <<"">>)
  ],
  [ok = aeconnector:push_tx(btc_conncetor(), Item) || Item <- Scheduled],
  ok.

pop_tx(_Config) ->
  {ok, Item} = aeconnector:pop_tx(btc_conncetor()),
  true = aeconnector_schedule:is_item(Item),
  ok.

send_tx(_Config) ->
  Payload = <<"Hyperchains trace">>,
  [ok = aeconnector:send_tx(btc_conncetor(), Payload) || _ <- lists:seq(0, 0)],
  ok.

disconnect(_Config) ->
  ok = aeconnector:disconnect(btc_conncetor()).

btc_conncetor() ->
  aeconnector_btc_full_node.
