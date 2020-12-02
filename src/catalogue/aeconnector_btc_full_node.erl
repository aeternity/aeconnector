%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc

%%% @end
-module(aeconnector_btc_full_node).

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
-export([connected/3, disconnected/3]).

-type block() :: aeconnector_block:block().

-type item() :: aeconnector_schedule:item().

%%%===================================================================
%%%  aeconnector behaviour
%%%===================================================================

-spec connect(map(), function()) -> {ok, pid()} | {error, term()}.
connect(Args, Callback) when is_map(Args), is_function(Callback) ->
  Data = data(Args, Callback),
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

%% By this way we can act as payment gateway
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
    %% RPC auth
    auth::list(),
    %% RPC url
    url::list(),
    %% RPC serial
    serial::non_neg_integer(),
    %% RPC seed
    seed::binary(),
    %% New mined block anouncement
    callback::function(),
    %% The current top block hash
    top::binary(),
    %% The current commitment hash
    tx::binary(),
    %% Request timeout
    timeout::integer(),
    %% Established connection timeout
    connect_timeout::integer(),
    %% Allowed redirect
    autoredirect::boolean(),
    %% Selected wallet
    wallet::binary(),
    %% Queued commitments list;
    queue::term(),
    %% Minimum balance to operate
    min::float(),
    %% Direct fee definition
    fee::float(),
    %% Estimated fee mode
    estimate:: undefined | unset | economical | conservative
  }).

-type data() :: #data{}.

init(Data) ->
  try
    {ok, Res, Data2} = request(<<"/">>, <<"getbestblockhash">>, [], Data),
    Top = result(Res),
    {ok, connected, top(Data2, Top)}
  catch _:_ ->
    {ok, disconnected, Data, [{state_timeout, 1000, connect}]}
  end.

callback_mode() ->
  [state_functions, state_enter].

terminate(_Reason, _State, _Data) ->
  ok.

%%%===================================================================
%%%  State machine callbacks
%%%===================================================================

connected(enter, _OldState, Data) ->
  lager:debug("~nBTC network is connected: ~p~n", [top(Data)]),
  {keep_state, Data, [{{timeout, sync}, 0, _EventContent = []}]};

connected({timeout, sync}, _, Data) ->
  Top = top(Data),
  {ok, Res, Data2} = request(<<"/">>, <<"getbestblockhash">>, [], Data),
  Hash = result(Res),
  Data4 =
    case Hash of
      Top ->
        Data2;
      _ ->
        {ok, Res2, Data3} = request(<<"/">>, <<"getblock">>, [Hash, _Verbosity = 2], Data2),
        Callback = callback(Data3),
        Callback(?MODULE, block(Res2)),
        ct:log("~nA hash: ~p the top: ~p~n",[Hash, Top]),
        lager:debug("~nBTC network is synched: ~p~n", [Top]),
        Data3
    end,
  {keep_state, Data4, [{{timeout, sync}, 1000, _EventContent = []}]};

connected({call, From}, {get_top_block}, Data) ->
  {ok, Res, Data2} = request(<<"/">>, <<"getbestblockhash">>, [], Data),
  Reply = result(Res),
  ok = gen_statem:reply(From, {ok, Reply}),
  {keep_state, Data2, []};

connected({call, From}, {get_block_by_hash, Hash}, Data) ->
  {ok, Res, Data2} = request(<<"/">>, <<"getblock">>, [Hash, _Verbosity = 2], Data),
  Reply = block(Res),
  ok = gen_statem:reply(From, {ok, Reply}),
  {keep_state, Data2, []};

connected({call, From}, {dry_send_tx, _Delegate, _Payload}, Data) ->
  Wallet = wallet(Data),
  Args = [_Minconf = 1, _Maxconf = 9999999, _Addresses = [], true, #{ <<"minimumAmount">> => min(Data) } ],
  {ok, Res, Data2} = request(<<"/wallet/", Wallet/binary>>, <<"listunspent">>, Args, Data),
  Listunspent = result(Res),
  Reply = Listunspent /= [],
  ok = gen_statem:reply(From, Reply),
  {keep_state, Data2, []};

connected({call, From}, {send_tx, _Delegate, Payload}, Data) ->
  Wallet = wallet(Data),
  %% Min confirmations is lower with idea to support "hot" balance refilling for validators;
  Args = [_Minconf = 1, _Maxconf = 9999999, _Addresses = [], true, #{ <<"minimumAmount">> => min(Data) }],
  {ok, Res, Data2} = request(<<"/wallet/", Wallet/binary>>, <<"listunspent">>, Args, Data),
  ct:log("~nlistunspent: ~p~n",[Res]),
  [Input|_] = result(Res),
  TxId = maps:get(<<"txid">>, Input),
  Vout = maps:get(<<"vout">>, Input),
  AmountIn = maps:get(<<"amount">>, Input),
  %% NOTE:
  %% a) We use the first available input which matches the criteria (the list will be updated due to the sender activity);
  %% b) txid from the Input;
  %% c) vout is set to 0 accordingly to unspendable protocol for null data txs;
  %% d) Payload is encoded into hex format;
  ToHex = fun(C) when C < 10 -> $0 + C; (C) -> $a + C - 10 end,
  HexData = << <<(ToHex(H)),(ToHex(L))>> || <<H:4,L:4>> <= Payload >>,
  Inputs = [#{<<"txid">> => TxId, <<"vout">> => Vout}],
  Outputs = #{<<"data">> => HexData},
  %% NOTE:
  %% The payment address is made from queued specification (otherwise to the same input)
  {Out, Data3} = out(Data),
  Payment =
    case Out of
      empty ->
        Address = maps:get(<<"address">>, Input),
        AmountOut = float_to_binary(AmountIn - fee(Data), [{decimals, 4}]),
        ct:log("~nDefault commitment (address: ~p, amount: ~p)~n", [Address, AmountOut]),
        #{Address => AmountOut};
      {value, Item} ->
        Address = aeconnector_schedule:address(Item),
        _Amount = aeconnector_schedule:amount(Item),
        _Fee = aeconnector_schedule:fee(Item),
        Comment = aeconnector_schedule:comment(Item),
        AmountOut = 0,
        %% TODO To calculate output;
        ct:log("~nScheduled commitment (address: ~p, amount: ~p, comment: ~p)~n", [Address, AmountOut, Comment]),
        #{Address => AmountOut}
    end,

  Outputs2 = maps:merge(Outputs, Payment),
  Args2 = [Inputs, Outputs2],
  {ok, Res2, Data3} = request(<<"/wallet/", Wallet/binary>>, <<"createrawtransaction">>, Args2, Data2),
  RawTx = result(Res2),
  ct:log("~ncreaterawtransaction: ~p~n",[Res2]),

  Args3 = [RawTx],
  {ok, Res3, Data4} = request(<<"/wallet/", Wallet/binary>>, <<"signrawtransactionwithwallet">>, Args3, Data3),
  ct:log("~nsignrawtransactionwithwallet: ~p~n",[Res3]),
  SignedTx = result(Res3),
  Complete = maps:get(<<"complete">>, SignedTx), true = Complete,
  Hex = maps:get(<<"hex">>, SignedTx),

  Args4 = [Hex],
  {ok, Res4, Data5} = request(<<"/wallet/", Wallet/binary>>, <<"sendrawtransaction">>, Args4, Data4),
  ct:log("~nsendrawtransaction: ~p~n",[Res4]),
  ok = gen_statem:reply(From, ok),
  {keep_state, Data5, []};

connected({call, From}, {push_tx, Item}, Data) ->
  Queue2 = queue:in(Item, queue(Data)),
  Data2 = queue(Data, Queue2),
  ok = gen_statem:reply(From, ok),
  {keep_state, Data2, []};

connected({call, From}, {pop_tx}, Data) ->
  Queue = queue(Data),
  try
    {{value, Item}, Queue2} = queue:out_r(Queue),
    ok = gen_statem:reply(From, {ok, Item}),
    Data2 = queue(Data, Queue2),
    {keep_state, Data2, []}
  catch E:R ->
    ok = gen_statem:reply(From, {error, {E, R}}),
    {keep_state, Data, []}
  end.

disconnected(enter, _OldState, Data) ->
  lager:debug("~nBTC network is disconnected ~n"),

  {keep_state, Data};

disconnected(state_timeout, _, Data) ->
  lager:debug("~nBTC network connection attempt......~n"),
  try
    {ok, Res, Data2} = request(<<"/">>, <<"getbestblockhash">>, [], Data),
    Top = result(Res),
    {next_state, connected, top(Data2, Top)}
  catch _:_ ->
    {keep_state, Data, [{state_timeout, 1000, connect}]}
  end;

disconnected(_, _, Data) ->
  {keep_state, Data, [postpone]}.

%%%===================================================================
%%%  Data access layer
%%%===================================================================

-spec out(data()) -> {{value, item()}, data()} | {empty, data()}.
out(Data) ->
  Queue = queue(Data),
  case queue:out(Queue) of
    {Res = {value, _Item}, Queue2} ->
      Data2 = queue(Data, Queue2),
      {Res, Data2};
    {Res = empty, _Queue2} ->
      {Res, Data}
  end.

-spec data(map(), function()) -> data().
data(Args, Callback) ->
  User = maps:get(<<"user">>, Args),
  Password = maps:get(<<"password">>, Args),
  Host = maps:get(<<"host">>, Args, <<"127.0.0.1">>),
  Port = maps:get(<<"port">>, Args, 8332),
  SSL = maps:get(<<"ssl">>, Args, false),
  Timeout = maps:get(<<"timeout">>, Args, 30000),
  ConTimeout = maps:get(<<"connect_timeout">>, Args, 3000),
  AutoRedirect = maps:get(<<"autoredirect">>, Args, true),
  Wallet = maps:get(<<"wallet">>, Args),
  Min = maps:get(<<"min">>, Args),
  Fee = maps:get(<<"fee">>, Args),
  URL = url(binary_to_list(Host), Port, SSL),
  Auth = auth(binary_to_list(User), binary_to_list(Password)),
  Serial = 0,
  Seed = erlang:md5(term_to_binary({Serial, node(), make_ref()})),
  #data{
    auth = Auth,
    url = URL,
    serial = Serial,
    seed = Seed,
    callback = Callback,
    timeout = Timeout,
    connect_timeout = ConTimeout,
    autoredirect = AutoRedirect,
    wallet = Wallet,
    queue = queue:new(),
    min = Min,
    fee = Fee
  }.

-spec auth(data()) -> binary().
auth(Data) ->
  Data#data.auth.

-spec timeout(data()) -> integer().
timeout(Data) ->
  Data#data.timeout.

-spec connect_timeout(data()) -> integer().
connect_timeout(Data) ->
  Data#data.connect_timeout.

-spec autoredirect(data()) -> boolean().
autoredirect(Data) ->
  Data#data.autoredirect.

-spec wallet(data()) -> binary().
wallet(Data) ->
  Data#data.wallet.

-spec min(data()) -> float().
min(Data) ->
  Data#data.min.

-spec fee(data()) -> float().
fee(Data) ->
  Data#data.fee.

-spec url(data()) -> binary().
url(Data) ->
  Data#data.url.

-spec seed(data()) -> binary().
seed(Data) ->
  Data#data.seed.

-spec seed(data(), binary()) -> data().
seed(Data, Seed) ->
  Data#data{ seed = Seed }.

-spec queue(data()) -> term().
queue(Data) ->
  Data#data.queue.

-spec queue(data(), term()) -> data().
queue(Data, Queue) ->
  Data#data{ queue = Queue}.

-spec callback(data()) -> function().
callback(Data) ->
  Data#data.callback.

-spec top(data()) -> binary().
top(Data) ->
  Data#data.top.

-spec top(data(), binary()) -> data().
top(Data, Top) ->
  Data#data{top = Top}.

%%-spec tx(data()) -> binary().
%%tx(Data) ->
%%  Data#data.tx.

%%-spec tx(data(), binary()) -> data().
%%tx(Data, Tx) ->
%%  Data#data{tx = Tx}.

%%%===================================================================
%%%  BTC protocol
%%%===================================================================

url(Host, Port, true = _SSL) when is_list(Host), is_integer(Port) ->
  path("https://", Host, Port);
url(Host, Port, _) when is_list(Host), is_integer(Port) ->
  path("http://", Host, Port).

path(Scheme, Host, Port) ->
  lists:concat([Scheme, Host, ":", Port]).

-spec request(binary(), binary(), list(), data()) -> {ok, map(), data()} | {error, term(), data()}.
request(Path, Method, Params, Data) ->
  Seed = seed(Data),
  %% TODO: Inc seed;
  DataUp = seed(Data, Seed),
  try
    Rpc = rpc(Method, Params, _Id = base64:encode(Seed)),
    Auth = auth(Data),
    Url = lists:concat([url(Data), binary_to_list(Path)]),
    Body = jsx:encode(Rpc),
    Headers = [
        {"Authorization", lists:concat(["Basic ", Auth])}
      ],
    Req = {Url, Headers, "application/json", Body},
    HTTPOpt = [
        {timeout, timeout(Data)},
        {connect_timeout, connect_timeout(Data)},
        {autoredirect, autoredirect(Data)}
      ],
    Opt = [],
    {ok, {{_, 200 = _Code, _}, _, Res}} = httpc:request(post, Req, HTTPOpt, Opt),
    lager:debug("Req: ~p, Res: ~p with URL: ~ts", [Req, Res, Url]),
    {ok, jsx:decode(list_to_binary(Res)), DataUp}
  catch E:R:S ->
    lager:error("Error: ~p Reason: ~p Stacktrace: ~p", [E, R, S]),
    {error, {E, R, S}, DataUp}
  end.

-spec auth(binary(), binary()) -> list().
auth(User, Password) ->
  base64:encode_to_string(lists:concat([User, ":", Password])).

-spec rpc(binary(), list(), binary()) ->
  {ok, map()} | {error, term()}.
rpc(Method, Params, Id) ->
  rpc(Method, Params, Id, <<"2.0">>).

-spec rpc(binary(), list(), binary(), binary()) ->
  {ok, map()} | {error, term()}.
rpc(Method, Params, Id, Version) ->
    #{
      <<"jsonrpc">> => Version,
      <<"method">> => Method,
      <<"params">> => Params,
      <<"id">> => Id
    }.

-spec result(map()) -> binary().
result(Response) ->
  maps:get(<<"result">>, Response).

-spec block(map()) -> block().
block(Response) ->
  Result = result(Response),
  Hash = maps:get(<<"hash">>, Result), true = is_binary(Hash),
  Height = maps:get(<<"height">>, Result), true = is_integer(Height),
  PrevHash = maps:get(<<"previousblockhash">>, Result), true = is_binary(PrevHash),
  %% TODO: To analyze the size field;
  Txs = [
    begin
      Vin = maps:get(<<"vin">>, Tx),
      Vout = maps:get(<<"vout">>, Tx),
      %% TODO: To extract Tx data;
      aeconnector_tx:test_tx(Vin, Vout) end|| Tx <- maps:get(<<"tx">>, Result)
  ],
  aeconnector_block:block(Height, Hash, PrevHash, Txs).