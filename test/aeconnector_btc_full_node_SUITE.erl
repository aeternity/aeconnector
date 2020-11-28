%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% This module executes BTC integration acceptance processes across "user" and "delegate" roles
%%% The process requires opened connection to the BTC network and divided into the next steps
%%% For "user" role (default):
%%% 1. Open connection to the network (connect);
%%% 2. Fetch the hash of the current best block (get_top_block);
%%% 3. Fetch the block data for predefined pin-pointed hash (get_block_by_hash);
%%% 4. Synchronize the new mined block (a time depends on BTC network: ~8 min) (sync)
%%% 5. Close connection (disconnect);
%%% For "delegate" role:
%%% 1. Open connection to the network (connect);
%%% 2. Check available inputs, balance (dry_send_tx);
%%% 3. Send commitment transaction with predefined payload (send_tx);
%%% 4. Put scheduled commitments into queue;
%%% 5. Send scheduled commitments
%%% 5. Close connection (disconnect);
%%% @end
-module(aeconnector_btc_full_node_SUITE).

-export([suite/0]).

-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([groups/0]).

-export([all/0]).

-export([connect/0, connect/1]).
-export([dry_send_tx/0, dry_send_tx/1, send_tx/0, send_tx/1]).
-export([get_top_block/0, get_top_block/1]).
-export([get_block_by_hash/0, get_block_by_hash/1]).
-export([synchronize/0, synchronize/1]).
-export([schedule/0, schedule/1]).
-export([disconnect/0, disconnect/1]).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
  [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
  ok = lager:start(),
  %% TODO: This data should be externally configured!
  Payload = <<"Hyperchains trace">>,
  GenesisHash = <<"0000000068d9d45579eeaf657ac6b446d8ec072a40b9cf7c0d566d57e20c5148">>,
  Setup = [{payload, Payload}, {test_hash, GenesisHash}],
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
groups() ->
  [
    {user, [sequence], [connect, get_top_block, get_block_by_hash, synchronize, disconnect]},
    {delegate, [sequence], [connect, dry_send_tx, send_tx, schedule, disconnect]}
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
  [{group, user}, {group, delegate}].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

connect() ->
  [].

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
connect(_Config) ->
  Args = #{
    <<"user">> => <<"hyperchains">>,
    <<"password">> => <<"qwerty">>,
    <<"host">> => <<"127.0.0.1">>,
    <<"private_key">> => <<"">>,
    <<"port">> => 8332,
    <<"ssl">> => false,
    <<"timeout">> => 30000,
    <<"connect_timeout">> => 3000,
    <<"autoredirect">> => true,
    <<"wallet">> => <<"Hyperchains">>,
    <<"min">> => 0.001,
    <<"fee">> => 0.0009
  },
  Callback = fun (_Con, Block) -> ct:log(info, "~nMined block: ~p~n", [Block]) end,
  {ok, Pid} = aeconnector:connect(btc_conncetor(), Args, Callback),
  {comment, Pid}.

get_top_block() ->
  [].

get_top_block(_Config) ->
  {ok, Hash} = aeconnector:get_top_block(btc_conncetor()),
  {comment, Hash}.

get_block_by_hash() ->
  [].

get_block_by_hash(Config) ->
  Hash = ?config(test_hash, Config),
  {ok, Block} = aeconnector:get_block_by_hash(btc_conncetor(), Hash),
  {comment, Block}.

synchronize() ->
  [].

synchronize(_Config) ->
  ok.

dry_send_tx() ->
  [].

dry_send_tx(_Config) ->
  Payload = <<"Hyperchains test trace">>,
  true = aeconnector:dry_send_tx(btc_conncetor(), <<"TEST">>, Payload),
  ok.

send_tx() ->
  [].

send_tx(_Config) ->
  Payload = <<"Hyperchains trace">>,
  ok = aeconnector:send_tx(btc_conncetor(), <<"TEST">>, Payload).

schedule() ->
  [].

schedule(_Config) ->
  ok.

disconnect() ->
  [].

disconnect(_Config) ->
  ok = aeconnector:disconnect(btc_conncetor()).

btc_conncetor() ->
  aeconnector_btc_full_node.