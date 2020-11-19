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

%% gen_statem.
-export([init/1]).
-export([terminate/3]).
-export([callback_mode/0]).

%% transitions
-export([connected/3, confirmed/3, updated/3, disconnected/3]).

-type block() :: aeconnector_block:block().

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

-spec send_tx(binary(), binary()) -> ok.
send_tx(Delegate, Payload) ->
  gen_statem:call(?MODULE, {send_tx, Delegate, Payload}).

-spec get_top_block() -> {ok, binary()} | {error, term()}.
get_top_block() ->
  gen_statem:call(?MODULE, {get_top_block}).

-spec get_block_by_hash(binary()) -> {ok, block()} | {error, term()}.
get_block_by_hash(Hash) ->
  gen_statem:call(?MODULE, {get_block_by_hash, Hash}).


-spec disconnect() -> ok.
disconnect() ->
  gen_statem:stop(?MODULE).

%%%===================================================================
%%%  gen_statem behaviour
%%%===================================================================

-record(data, {
    %% RPC auth (bitcoin.conf)
    auth::list(),
    %% RPC url (bitcoin.conf)
    url::list(),
    %% RPC serial
    serial::non_neg_integer(),
    %% RPC seed
    seed::binary(),
    %% New mained block anouncement
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
    %% Selected wallet (balance > 0)
    wallet::binary(),
    %% Bitcoin sender address
%%    from::binary(),
    %% Bitcoin receiver address (from by default)
    to::binary(),
    %% Permitted amount to operate;
    min::float(),
    %% Calculated fee
    fee::float()
  }).

-type data() :: #data{}.

init(Data) ->
  %% TODO: to perform get top and info reqs;
  %% getblockchaininfo, getnetworkinfo, and getwalletinfo
  %% TODO: pruneblockchain at height of genesis;
  {ok, Res, DataUp} = request(<<"/">>, <<"getblockchaininfo">>, [], Data),
  lager:debug("~nBTC network: ~p~n", [Res]),
  {ok, connected, DataUp, []}.

callback_mode() ->
  [state_functions, state_enter].

terminate(_Reason, _State, _Data) ->
  ok.

%%%===================================================================
%%%  State machine callbacks
%%%===================================================================

connected(enter, _OldState, Data) ->
  {ok, _Response, Data2} = request(<<"/">>, <<"getbestblockhash">>, [], Data),
  _Top = top(Data),
  {keep_state, Data2};

connected({call, From}, {get_top_block}, Data) ->
  {ok, Response, Data2} = request(<<"/">>, <<"getbestblockhash">>, [], Data),
  Reply = top_block(Response),
  ok = gen_statem:reply(From, {ok, Reply}),
  {keep_state, Data2, []};

connected({call, From}, {get_block_by_hash, Hash}, Data) ->
  {ok, Response, Data2} = request(<<"/">>, <<"getblock">>, [Hash, _Verbosity = 2], Data),
  Reply = block(Response),
  ok = gen_statem:reply(From, {ok, Reply}),
  {keep_state, Data2, []};

connected({call, From}, {dry_send_tx, _Delegate, _Payload}, Data) ->
  Wallet = wallet(Data),
  %% _Addresses = [from(Data)],
  Args = [_Minconf = 6, _Maxconf = 9999999, _Addresses = [], true, #{ <<"minimumAmount">> => min(Data) } ],
  {ok, Response, Data2} = request(<<"/wallet/", Wallet/binary>>, <<"listunspent">>, Args, Data),
  Listunspent = listunspent(Response),
  Reply = Listunspent /= [],
  ok = gen_statem:reply(From, Reply),
  {keep_state, Data2, []};

connected({call, From}, {send_tx, _Delegate, Payload}, Data) ->
  Wallet = wallet(Data),
  %% Min confirmations is lower with idea to "hot" refilling of balance for operators;
  %% _Addresses = [from(Data)],
  Args = [_Minconf = 1, _Maxconf = 9999999, _Addresses = [], true, #{ <<"minimumAmount">> => min(Data) }],
  {ok, Response, Data2} = request(<<"/wallet/", Wallet/binary>>, <<"listunspent">>, Args, Data),
  ct:log("~nlistunspent: ~p~n",[Response]),
  TxId = txid(Response), Vout = vout(Response), Amount = amount(Response), Address = address(Response),
  %% NOTE:
  %% a) We use the first available input which matches the criteria (the list is updated each time due to sender activity);
  %% b) txid is the Input;
  %% c) vout is set to 0 accordingly to unspendable protocol for null data txs;
  %% d) Payload is encoded into Bitcoin hex format;
  Hex = fun(C) when C < 10 -> $0 + C; (C) -> $a + C - 10 end,
  HexData = << <<(Hex(H)),(Hex(L))>> || <<H:4,L:4>> <= Payload >>,
  Args2 = [[#{<<"txid">> => TxId, <<"vout">> => Vout}], #{Address => Amount - fee(Data), <<"data">> => HexData}],
  {ok, Response2, Data3} = request(<<"/wallet/", Wallet/binary>>, <<"createrawtransaction">>, Args2, Data2),
  RawTx = createrawtransaction(Response2),
  ct:log("~ncreaterawtransaction: ~p~n",[Response2]),
  Args3 = [RawTx],
  {ok, Response3, Data4} = request(<<"/wallet/", Wallet/binary>>, <<"signrawtransactionwithwallet">>, Args3, Data3),
  ct:log("~nsignrawtransactionwithwallet: ~p~n",[Response3]),
  SignedTx = signrawtransactionwithwallet(Response3),
  Args4 = [SignedTx],
  {ok, Response4, Data5} = request(<<"/wallet/", Wallet/binary>>, <<"sendrawtransaction">>, Args4, Data4),
  ct:log("~nsendrawtransaction: ~p~n",[Response4]),
  ok = gen_statem:reply(From, ok),
  {keep_state, Data5, []}.

confirmed(enter, _OldState, Data) ->
  %% TODO: To supply HTTP callback for miner;
  %% TODO: Web hook for telegram;
  {next_state, connected, Data}.

updated(enter, _OldState, Data) ->
  {next_state, connected, Data}.

disconnected(enter, _OldState, Data) ->
  {keep_state, Data};

disconnected(_, _, Data) ->
  {keep_state, Data, [postpone]}.

%%%===================================================================
%%%  Data access layer
%%%===================================================================

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
%%  From = maps:get(<<"from">>, Args),
%%  To = maps:get(<<"to">>, Args, From),
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
%%    from = From,
%%    to = To,
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

%%-spec from(data()) -> binary().
%%from(Data) ->
%%  Data#data.from.

%%-spec to(data()) -> binary().
%%to(Data) ->
%%  Data#data.to.

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

%%-spec callback(data()) -> function().
%%callback(Data) ->
%%  Data#data.callback.

-spec top(data()) -> binary().
top(Data) ->
  Data#data.top.

%%-spec top(data(), binary()) -> data().
%%top(Data, Top) ->
%%  Data#data{top = Top}.

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

-spec top_block(map()) -> binary().
top_block(Response) ->
  Res = maps:get(<<"result">>, Response), true = is_binary(Res),
  Res.

-spec block(map()) -> block().
block(Response) ->
  Result = maps:get(<<"result">>, Response),
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

-spec listunspent(map()) -> float().
listunspent(Response) ->
  Result = maps:get(<<"result">>, Response), true = is_list(Result),
  Result.

-spec txid(map()) -> binary().
txid(Response) ->
  [H|_] = maps:get(<<"result">>, Response),
  Result = maps:get(<<"txid">>, H), true = is_binary(Result),
  Result.

-spec amount(map()) -> binary().
amount(Response) ->
  [H|_] = maps:get(<<"result">>, Response),
  Result = maps:get(<<"amount">>, H), true = is_float(Result),
  Result.

-spec address(map()) -> binary().
address(Response) ->
  [H|_] = maps:get(<<"result">>, Response),
  Result = maps:get(<<"address">>, H), true = is_binary(Result),
  Result.

-spec vout(map()) -> integer().
vout(Response) ->
  [H|_] = maps:get(<<"result">>, Response),
  Result = maps:get(<<"vout">>, H), true = is_integer(Result),
  Result.

-spec createrawtransaction(map()) -> binary().
createrawtransaction(Response) ->
  Result = maps:get(<<"result">>, Response), true = is_binary(Result),
  Result.

-spec signrawtransactionwithwallet(map()) -> binary().
signrawtransactionwithwallet(Response) ->
  Result = maps:get(<<"result">>, Response),
  Complete = maps:get(<<"complete">>, Result), true = Complete,
  Hex = maps:get(<<"hex">>, Result), true = is_binary(Hex),
  Hex.