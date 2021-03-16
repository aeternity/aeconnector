%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% https://en.wikipedia.org/wiki/Media_control_symbols
%%% @end
-module(aeconnector_offline).

%% API
-export([]).

-behaviour(aeconnector).
-behaviour(gen_statem).

%% API.
-export([connect/2]).
-export([send_tx/2, dry_send_tx/2]).
-export([get_top_block/0, get_block_by_hash/1]).
-export([disconnect/0]).

-export([push_tx/1, pop_tx/0]).

%% gen_statem.
-export([init/1]).
-export([terminate/3]).
-export([callback_mode/0]).

%% transitions
-export([played/3, paused/3, stopped/3]).

-type block() :: aeconnector_block:block().
-type tx() :: aeconnector_tx:tx().

-type item() :: aeconnector_schedule:item().

-type script() :: map().

%%%===================================================================
%%% Script processing
%%%===================================================================

-spec compile(data(), script()) -> data().
compile(Data, Script) ->
  Commands = queue:new(), Data2 = commands(Data, Commands),
  I = maps:iterator(Script), Next = maps:next(I),

  next(script(Data2, Script), Next).

-spec next(data(), none | {binary(), term(), term()}) -> data().
next(Data, none) ->
  Data;
next(Data, {<<"block">>, Value, Iterator}) ->
  BlockTime = maps:get(<<"blocktime">>, Value),
  Command = {{timeout, <<"block">>}, BlockTime * 1000, Value},

  Data2 = in(Data, Command), next(Data2, maps:next(Iterator));
next(Data, {_Key, _Value, Iterator}) ->
  next(Data, maps:next(Iterator)).

%%%===================================================================
%%%  aeconnector behaviour
%%%===================================================================

-spec connect(map(), function()) -> {ok, pid()} | {error, term()}.
connect(Args, Callback) when is_map(Args), is_function(Callback) ->
  Data = callback(args(data(), Args), Callback),
  gen_statem:start({local, ?MODULE}, ?MODULE, Data, []).

-spec dry_send_tx(binary(), binary()) -> boolean().
dry_send_tx(Delegate, Payload) ->
  gen_statem:call(?MODULE, {dry_send_tx, Delegate, Payload}).

-spec send_tx(binary(), binary()) -> ok | {error, term()}.
send_tx(Delegate, Payload) ->
  gen_statem:call(?MODULE, {send_tx, Delegate, Payload}).

-spec get_top_block() -> {ok, binary()} | {error, term()}.
get_top_block() ->
  gen_statem:call(?MODULE, {get_top_block}).

-spec get_block_by_hash(binary()) -> {ok, block()} | {error, term()}.
get_block_by_hash(Hash) ->
  gen_statem:call(?MODULE, {get_block_by_hash, Hash}).

%% NOTE: Regular sender API
-spec push_tx(item()) -> ok.
push_tx(Item) ->
  gen_statem:call(?MODULE, {push_tx, Item}).

-spec pop_tx() -> {ok, item()} | {error, term()}.
pop_tx() ->
  gen_statem:call(?MODULE, {pop_tx}).

-spec disconnect() -> ok.
disconnect() ->
  gen_statem:stop(?MODULE).

%%%===================================================================
%%%  gen_statem behaviour
%%%===================================================================

-record(data, {
  callback :: function(),
  pool :: [tx()],
  stack :: [block()],
  script :: script(),
  commands :: term()
}).

-type data() :: #data{}.

init(Data) ->
  Script = script(Data),
  Data2 = compile(Data, Script),

  {{value, Command}, Data3} = out(Data2),
  {ok, played, Data3, [Command]}.

callback_mode() ->
  [state_functions, state_enter].

terminate(_Reason, _State, _Data) ->
  ok.

%%%===================================================================
%%%  State machine
%%%===================================================================

played(enter, _OldState, Data) ->
  lager:info("~nOffline connector in state played (commands length: ~p)~n", [len(Data)]),

  {keep_state, Data, []};

played({timeout, <<"block">>}, EventContent, Data) ->
  Block = block(EventContent),
  Pool = pool(Data), Txs = lists:append(Pool, aeconnector_block:txs(Block)),
  MinedBlock = aeconnector_block:txs(Block, Txs),
  Callback = callback(Data), catch(Callback(?MODULE, MinedBlock)),

  {keep_state, Data, []};

played({call, From}, {get_top_block}, Data) ->
  [Top|_] = stack(Data), Hash  = aeconnector_block:hash(Top),

  ok = gen_statem:reply(From, {ok, Hash}),
  {keep_state, Data, []};

played({call, From}, {get_block_by_hash, Hash}, Data) ->
  Stack  = stack(Data), Block = lists:keyfind(Hash, aeconnector_block:hash(), Stack),

  ok = gen_statem:reply(From, {ok, Block}),
  {keep_state, Data, []};

played({call, From}, {dry_send_tx, Delegate, Payload}, Data) ->
  Tx = aeconnector_tx:tx(Delegate, Payload),

  ok = gen_statem:reply(From, aeconnector_tx:is_tx(Tx)),
  {keep_state, Data, []};

played({call, From}, {send_tx, Delegate, Payload}, Data) ->
  Tx = aeconnector_tx:tx(Delegate, Payload),
  Pool = pool(Data),
  Data2 = pool(Data, [Tx|Pool]),

  ok = gen_statem:reply(From, ok),
  {keep_state, Data2, []};

played({call, _From}, {push_tx, _Item}, Data) ->

  throw('not implemented'),
  {keep_state, Data, []};

played({call, _From}, {pop_tx}, Data) ->

  throw('not implemented'),
  {keep_state, Data, []}.

paused(enter, _OldState, Data) ->
  lager:info("~nOffline connector in state paused (commands length: ~p)~n", [len(Data)]),

  {keep_state, Data};

paused(_, _, Data) ->
  {keep_state, Data, [postpone]}.

stopped(enter, _OldState, Data) ->
  lager:info("~nOffline connector in state stopped (commands length: ~p)~n", [len(Data)]),

  {keep_state, Data};

stopped(_, _, Data) ->
  {keep_state, Data, [postpone]}.

%%%===================================================================
%%%  Data access
%%%===================================================================
-spec data() -> data().
data() ->
  #data{ stack = [], pool = [] }.

-spec args(data(), map()) -> data().
args(Data, Args) ->
  Priv = aeconnector:priv_dir(),
  Def = filename:join(Priv, "offline.yaml"),

  Path = maps:get(<<"script">>, Args, Def),

  Data#data{ script = file(Path, _Schema = []) }.

-spec pool(data()) -> [tx()].
pool(Data) ->
  Data#data.pool.

-spec pool(data(), [tx()]) -> data().
pool(Data, Txs) when is_list(Txs) ->
  Data#data{ pool = Txs }.

-spec stack(data()) -> [block()].
stack(Data) ->
  Data#data.stack.

-spec stack(data(), [block()]) -> data().
stack(Data, Stack) ->
  Data#data{ stack = Stack }.

-spec script(data()) -> script().
script(Data) ->
  Data#data.script.

-spec script(data(), script()) -> data().
script(Data, Script) ->
  Data#data{ script = Script }.

-spec callback(data()) -> function().
callback(Data) ->
  Data#data.callback.

-spec callback(data(), function()) -> data().
callback(Data, Callback) when is_function(Callback) ->
  Data#data{ callback = Callback }.

-spec commands(data()) -> term().
commands(Data) ->
  Data#data.commands.

-spec commands(data(), term()) -> data().
commands(Data, Commands) ->
  Data#data{ commands = Commands}.

-spec out(data()) -> {{value, item()}, data()} | {empty, data()}.
out(Data) ->
  Commands = commands(Data),
  case queue:out(Commands) of
    {Res = {value, _Item}, Commands2} ->
      Data2 = commands(Data, Commands2),
      {Res, Data2};
    {Res = empty, _Commands2} ->
      {Res, Data}
  end.

-spec in(data(), item()) -> data().
in(Data, Item) ->
  Commands2 = queue:in(Item, commands(Data)),
  commands(Data, Commands2).

-spec len(data()) -> integer().
len(Data) ->
  Commands = commands(Data),
  queue:len(Commands).

%%%===================================================================
%%%  Blocks
%%%===================================================================

-spec block(map()) -> block().
block(Obj) ->
  Hash = maps:get(<<"hash">>, Obj), PrevHash = maps:get(<<"prev">>, Obj),
  Height = maps:get(<<"height">>, Obj),
  Txs = [tx(Tx)||Tx <- maps:get(<<"tx">>, Obj)],

  aeconnector_block:block(Height, Hash, PrevHash, Txs).

-spec tx(map()) -> tx().
tx(Obj) ->
  Account = maps:get(<<"account">>, Obj),
  Payload = maps:get(<<"payload">>, Obj),

  aeconnector_tx:test_tx(Account, Payload).

%%%===================================================================
%%%  Filesystem
%%%===================================================================

-spec file(Path::path(), Schema::[term()]) ->
  success(script()) | failure(term(), term()).
file(Path, _Schema) ->
  try
    Opt = [{str_node_as_binary, true}, {map_node_format, map}],
    [File] = yamerl_constr:file(Path, Opt),
%%    {ok, _} = jesse:validate(Schema, Datasheet)
    {ok, File}
  catch E:R ->
    {error, {E, R}}
  end.
