-module(aeconnector_btc_full_node_SUITE).

-export([suite/0]).

-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([groups/0]).

-export([all/0]).

-export([connect/0, connect/1]).
-export([dry_send_tx/0, dry_send_tx/1, send_tx/0, send_tx/1]).
-export([disconnect/0, disconnect/1]).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
  [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
  [{delegate, <<"TEST">>}|Config].

end_per_suite(_Config) ->
  ok.

init_per_group(_GroupName, Config) ->
  Config.

end_per_group(_GroupName, _Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  ok = lager:start(),
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
    {commit, [], [connect,  disconnect]}
%%    {commit, [], [connect, dry_send_tx, send_tx, disconnect]}
%%    {fetch, [sequence], [get_top_block, get_block_by_hash]},
%%    {transition, [sequence], []}
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
  [{group, commit}].

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
    <<"autoredirect">> => true
  },
  Callback = fun (_Con, Block) -> ct:log(info, "~nMined block: ~p~n", [Block]) end,
  {ok, _Pid} = aeconnector_btc_full_node:connect(Args, Callback),
  ok.

dry_send_tx() ->
  [].

dry_send_tx(Config) ->
  Delegate = ?config(delegate, Config),
  Commitment = <<"TEST">>,
  ok = aeconnector_btc_full_node:dry_send_tx(Delegate, Commitment).

send_tx() ->
  [].

send_tx(Config) ->
  Delegate = ?config(delegate, Config),
  Commitment = <<"TEST">>,
  ok = aeconnector_btc_full_node:send_tx(Delegate, Commitment).

disconnect() ->
  [].

disconnect(_Config) ->
  ok = aeconnector_btc_full_node:disconnect().