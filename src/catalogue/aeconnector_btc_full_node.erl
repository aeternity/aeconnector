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
-export([send_tx/1, dry_send_tx/1]).
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
-type tx() :: aeconnector_tx:tx().

-type item() :: aeconnector_schedule:item().

%%%===================================================================
%%%  aeconnector behaviour
%%%===================================================================
-spec connect(map(), function()) -> {ok, pid()} | {error, term()}.
connect(Args, Callback) when is_map(Args), is_function(Callback) ->
  Data = callback(args(data(), Args), Callback),
  gen_statem:start({local, ?MODULE}, ?MODULE, Data, []).

-spec dry_send_tx(binary()) -> boolean().
dry_send_tx(Payload) ->
  gen_statem:call(?MODULE, {dry_send_tx, Payload}).

-spec send_tx(binary()) -> ok | {error, term()}.
send_tx(Payload) ->
  gen_statem:call(?MODULE, {send_tx, Payload}).

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
    %% Bitcoin node RPC credentials
    auth :: list(),
    %% Bitcoin node RPC connection address
    url :: list(),
    %% RPC serial
    serial :: non_neg_integer(),
    %% RPC seed
    seed :: binary(),
    %% Announcement of the best block
    callback :: function(),
    height :: non_neg_integer(),
    %% The current best block hash
    hash :: binary(),
    %% URL to POST rendered template
    webhook :: list(),
    %% Path to the template file
    template :: list(),
    %% Request timeout
    timeout = 5000 :: integer(),
    %% The current selected wallet
    wallet :: binary(),
    %% The current available balance
    balance = 0.0 :: float(),
    %% The Bitcoin address of a sender
    address :: binary(),
    %% The private key of a sender
    privatekey :: binary(),
    %% Amount to send
    amount = 0.0 :: float(),
    %% The transaction fee
    fee = conservative :: float() | unset | economical | conservative,
    %% FIFO queue of scheduled payments
    queue :: term()
  }).

-type data() :: #data{}.

init(Data) ->
  %% The sensitive flag is used by specialized processes (sign keys, password holders, etc.)
  process_flag(sensitive, true),
  try
    %% Template preparation
    (webhook(Data) == undefined) orelse compile(Data),
    %% Bitcoin network request (informational purposes only)
    {ok, Info, Data2} = getblockchaininfo(Data),
    lager:info("~nBTC network info: ~p~n", [Info]),
    SyncTimeOut = {{timeout, sync}, 0, _EventContent = []},
    {ok, connected, Data2, [SyncTimeOut]}
  catch E:R:S ->
    lager:info("~nBTC network crash: ~p ~p ~p~n", [E, R, S]),
    ConnectTimeOut = {state_timeout, 1000, connect},
    {ok, disconnected, Data, [ConnectTimeOut]}
  end.

callback_mode() ->
  [state_functions, state_enter].

terminate(_Reason, _State, _Data) ->
  ok.

%%%===================================================================
%%%  State machine callbacks
%%%===================================================================
connected(enter, _OldState, Data) ->
  %% TODO Announce http callback
  {keep_state, Data, []};

connected({timeout, sync}, _, Data) ->
  Top = hash(Data),
  {ok, Hash, Data2} = getbestblockhash(Data),
  Data5 =
    case Hash of
      Top ->
        Data2;
      _ ->
        {ok, Block, Data3} = getblock(Hash, _Verbosity = 2, Data),
        Callback = callback(Data3),
        catch(Callback(?MODULE, Block)),
        Data4 = balance(top(Data3, Block), 0.0483),
        lager:info("~nBTC network is synched on: ~p~n", [Hash]),
        report(Data4, _Connected = true),
        Data4
    end,
  {keep_state, Data5, [{{timeout, sync}, 1000, _EventContent = []}]};

connected({call, From}, {get_top_block}, Data) ->
  {ok, HexHash, Data2} = getbestblockhash(Data), Hash = aeconnector:from_hex(HexHash),

  ok = gen_statem:reply(From, {ok, Hash}),
  {keep_state, Data2, []};

connected({call, From}, {get_block_by_hash, Hash}, Data) ->
  HexHash = aeconnector:to_hex(Hash),
  {ok, Block, Data2} = getblock(HexHash, _Verbosity = 2, Data),
  Reply = Block,
  ok = gen_statem:reply(From, {ok, Reply}),
  {keep_state, Data2, []};

connected({call, From}, {push_tx, Item}, Data) ->
  Data2 = in(Data, Item),
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
  end;

connected({call, From}, {dry_send_tx, _Payload}, Data) ->
  %% NOTE: A number of confirmations has increased to make sure that validator's wallet is ready;
  MinConf = 1, MaxConf = 9999999,
  Address = address(Data),
  Amount = amount(Data),
  {ok, Listunspent, Data2} = listunspent(MinConf, MaxConf, [Address], true, #{ <<"minimumAmount">> => Amount }, Data),
  ok = gen_statem:reply(From, Listunspent /= []),
  {keep_state, Data2, []};

connected({call, From}, {send_tx, Payload}, Data) ->
  %% Min confirmations is lower with idea to support "hot" balance update for validators;
  MinConf = 1, MaxConf = 9999999,
  Address = address(Data),
  Amount = amount(Data),
  {ok, Listunspent, Data2} = listunspent(MinConf, MaxConf, [Address], true, #{ <<"minimumAmount">> => Amount }, Data),
  lager:info("~nListunspent: ~p~n",[Listunspent]),
  try
    Listunspent == [] andalso throw(<<"Listunspent is empty ">>),
    [Unspent|_] = Listunspent,
    lager:info("~nUnspent: ~p~n",[Unspent]),
    TxId = maps:get(<<"txid">>, Unspent), Vout = maps:get(<<"vout">>, Unspent), AmountIn = maps:get(<<"amount">>, Unspent),
    %% NOTE:
    %% a) We use the first available input which matches the criteria (the list will be updated due to the sender activity);
    %% b) txid from the Input;
    %% c) vout is set to 0 accordingly to unspendable protocol for null data tx'ss;
    %% d) Payload is encoded into hex format;
    HexPayload = aeconnector:to_hex(Payload),
    Inputs = [#{<<"txid">> => TxId, <<"vout">> => Vout}],
    Outputs = #{<<"data">> => HexPayload},
    %% NOTE:
    %% The payment address is made from queued specification (otherwise the same input is used)
    {Out, Data3} = out(Data2),
    Payment =
      case Out of
        empty ->
          Fee = fee(Data3), Amount1 = (Amount - Fee), Amount2 = AmountIn - Amount1,
          Out1 = aeconnector:amount(Amount1),
          Out2 = aeconnector:amount(Amount2),
          Template = lists:flatten(io_lib:format("Cycled commitment: address: ~s, amount: ~f, fee: ~f, out1: ~s, out2: ~s",
            [binary_to_list(Address), Amount, Fee, binary_to_list(Out1), binary_to_list(Out2)])),
          lager:info(Template),
          #{ Address => Out1, Address => Out2 };
        {value, Item} ->
          ScheduledAddress = aeconnector_schedule:address(Item),
          ScheduledAmount = aeconnector_schedule:amount(Item),
          ScheduledFee = aeconnector_schedule:fee(Item),
          Comment = aeconnector_schedule:comment(Item),
          %% TODO To calculate output;
          Template = "~nRegular commitment (address: ~p, amount: ~p, comment: ~p)~n",
          lager:info(Template, [ScheduledAddress, ScheduledAmount, ScheduledFee, Comment]),
          #{ ScheduledAddress => 0 }
      end,

    Outputs2 = maps:merge(Outputs, Payment),
    {ok, RawTx, Data4} = createrawtransaction(Inputs, Outputs2, Data3),
    lager:info("~ncreaterawtransaction: ~p~n",[RawTx]),

    {ok, Hex, Data5} = signrawtransactionwithkey(RawTx, Data4),
    lager:info("~signrawtransactionwithkey: ~p~n",[Hex]),

    {ok, Hash, Data6} = sendrawtransaction(Hex, Data5),
    lager:info("~nsendrawtransaction: ~p~n",[Hash]),
    %% TODO Announce http callback with commitment TxId
    %% The commitment hash announcement %% TODO to send via HTTP
    ok = gen_statem:reply(From, ok),

    %% TODO TO update balance at this line
    Balance = 0.0483,
    {keep_state, balance(Data6, Balance), []}
  catch E:R ->
    ok = gen_statem:reply(From, {error, {E, R}}),
    {keep_state, Data2, []}
  end.

disconnected(enter, _OldState, Data) ->
  %% TODO Announce http callback
%%  report(Data, _Connected = false),
  {keep_state, Data};

disconnected(state_timeout, _, Data) ->
  lager:info("~nBTC network connection attempt......~n"),
  try
    Hash = hash(Data),
    {ok, Block, Data2} = getblock(Hash, _Verbosity = 2, Data),
    {next_state, connected, top(Data2, Block)}
  catch _:_ ->
    {keep_state, Data, [{state_timeout, 1000, connect}]}
  end;

disconnected(_, _, Data) ->
  {keep_state, Data, []}.

%%%===================================================================
%%%  Data access layer
%%%===================================================================
-spec data() -> data().
data() ->
  #data{ queue = queue:new() }.

-spec args(data(), map()) -> data().
args(Data, Args) ->
  User = maps:get(<<"user">>, Args),
  Password = maps:get(<<"password">>, Args),
  Host = maps:get(<<"host">>, Args, <<"127.0.0.1">>),
  Port = maps:get(<<"port">>, Args, 8332),
  SSL = maps:get(<<"ssl">>, Args, false),
  Wallet = maps:get(<<"wallet">>, Args),
  Address = maps:get(<<"address">>, Args),
  PrivateKey = maps:get(<<"privatekey">>, Args),
  Amount = maps:get(<<"amount">>, Args),

  URL = url(binary_to_list(Host), Port, SSL),
  Auth = auth(binary_to_list(User), binary_to_list(Password)),
  Serial = 0,
  Seed = erlang:md5(term_to_binary({Serial, node(), make_ref()})),

  Priv = aeconnector:priv_dir(),
  Template = filename:join(Priv, "btc_full_node_telegram.dtl"),
  Data2 =
    Data#data{
      auth = Auth,
      url = URL,
      serial = Serial,
      seed = Seed,
      wallet = Wallet,
      address = Address,
      privatekey = PrivateKey,
      amount = Amount,
      template = Template
    },
  I = maps:iterator(Args), Next = maps:next(I),
  next(Data2, Next).

%% Non obligatory options setup
next(Data, none) ->
  Data;

next(Data, {<<"webhook">>, V, I}) ->
  WebHook = binary_to_list(V),
  next(Data#data{ webhook = WebHook }, maps:next(I));

next(Data, {<<"template">>, V, I}) ->
  Template = binary_to_list(V),
  next(Data#data{ template = Template }, maps:next(I));

next(Data, {<<"timeout">>, V, I}) ->
  next(Data#data{ timeout = V }, maps:next(I));

next(Data, {<<"fee">>, V, I}) when is_float(V) ->
  next(Data#data{ fee = V }, maps:next(I));

next(Data, {<<"fee">>, V, I}) when V == <<"unset">>;
                                   V == <<"economical">>;
                                   V == <<"conservative">> ->
  Fee = binary_to_atom(V, latin1),
  next(Data#data{ fee = Fee }, maps:next(I));

next(Data, {_, _, I}) ->
  next(Data, maps:next(I)).

-spec auth(data()) -> binary().
auth(Data) ->
  Data#data.auth.

-spec hash(data()) -> binary().
hash(Data) ->
  Data#data.hash.

-spec height(data()) -> binary().
height(Data) ->
  Data#data.height.

-spec top(data(), block()) -> data().
top(Data, Block) ->
  Hash = aeconnector_block:hash(Block), Height = aeconnector_block:height(Block),
  Data#data{ hash = Hash, height = Height }.

-spec webhook(data()) -> list().
webhook(Data) ->
  Data#data.webhook.

-spec template(data()) -> list().
template(Data) ->
  Data#data.template.

-spec timeout(data()) -> integer().
timeout(Data) ->
  Data#data.timeout.

-spec wallet(data()) -> binary().
wallet(Data) ->
  Data#data.wallet.

-spec balance(data()) -> float().
balance(Data) ->
  Data#data.balance.

-spec balance(data(), float()) -> data().
balance(Data, Balance) ->
  Data#data{ balance = Balance }.

-spec address(data()) -> binary().
address(Data) ->
  Data#data.address.

-spec privatekey(data()) -> binary().
privatekey(Data) ->
  Data#data.privatekey.

-spec amount(data()) -> float().
amount(Data) ->
  Data#data.amount.

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

-spec callback(data()) -> function().
callback(Data) ->
  Data#data.callback.

-spec callback(data(), function()) -> data().
callback(Data, Callback) when is_function(Callback) ->
  Data#data{ callback = Callback }.

-spec queue(data()) -> term().
queue(Data) ->
  Data#data.queue.

-spec queue(data(), term()) -> data().
queue(Data, Queue) ->
  Data#data{ queue = Queue}.

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

-spec in(data(), item()) -> data().
in(Data, Item) ->
  Queue2 = queue:in(Item, queue(Data)),
  queue(Data, Queue2).

%%-spec tx(data()) -> binary().
%%tx(Data) ->
%%  Data#data.tx.

%%-spec tx(data(), binary()) -> data().
%%tx(Data, Tx) ->
%%  Data#data{tx = Tx}.

%%%===================================================================
%%%  HTTP protocol
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
    HTTPOpt = [{timeout, timeout(Data)}],
    Opt = [],
    {ok, {{_, 200 = _Code, _}, _, Res}} = httpc:request(post, Req, HTTPOpt, Opt),
    lager:info("Req: ~p, Res: ~p with URL: ~ts", [Req, Res, Url]),
    {ok, jsx:decode(list_to_binary(Res), [return_maps]), DataUp}
  catch E:R:S ->
    lager:info("Error: ~p Reason: ~p Stacktrace: ~p", [E, R, S]),
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

%%%===================================================================
%%%  BTC protocol
%%%===================================================================
-spec getblockchaininfo(data()) -> {ok, map(), data()} | {error, term()}.
getblockchaininfo(Data) ->
  try
    {ok, Res, Data2} = request(<<"/">>, <<"getblockchaininfo">>, [], Data),
    Info = result(Res),
    {ok, Info, Data2}
  catch E:R ->
    {error, {E, R}}
  end.

-spec getbestblockhash(data()) -> {ok, binary(), data()} | {error, term()}.
getbestblockhash(Data) ->
  try
    {ok, Res, Data2} = request(<<"/">>, <<"getbestblockhash">>, [], Data),
    Hash = result(Res),
    {ok, Hash, Data2}
  catch E:R ->
    {error, {E, R}}
  end.

-spec getblock(binary(), integer(), data()) -> {ok, block(), data()} | {error, term()}.
getblock(Hash, Verbosity, Data) ->
  try
    {ok, Res, Data2} = request(<<"/">>, <<"getblock">>, [Hash, Verbosity], Data),
    Block = block(result(Res)),
    {ok, Block, Data2}
  catch E:R ->
    {error, {E, R}}
  end.

-spec listunspent(integer(), integer(), list(), boolean(), map(), data()) -> {ok, list(), data()} | {error, term()}.
listunspent(Minconf, Maxconf, Addresses, Unsafe, Query, Data) ->
  try
    Wallet = wallet(Data),
    Args = [Minconf, Maxconf, Addresses, Unsafe, Query],
    {ok, Res, Data2} = request(<<"/wallet/", Wallet/binary>>, <<"listunspent">>, Args, Data),
    Listunspent = result(Res),
    {ok, Listunspent, Data2}
  catch E:R ->
    {error, {E, R}}
  end.

-spec createrawtransaction(list(), map(), data()) -> {ok, binary(), data()} | {error, term()}.
createrawtransaction(Inputs, Outputs, Data) ->
  try
    Wallet = wallet(Data),
    {ok, Res, Data2} = request(<<"/wallet/", Wallet/binary>>, <<"createrawtransaction">>, [Inputs, Outputs], Data),
    RawTx = result(Res),
    {ok, RawTx, Data2}
  catch E:R ->
    {error, {E, R}}
  end.

-spec signrawtransactionwithkey(binary(), data()) -> {ok, binary(), data()} | {error, term()}.
signrawtransactionwithkey(RawTx, Data) ->
  try
    PrivKey = privatekey(Data),
    {ok, Res, Data2} = request(<<"/">>, <<"signrawtransactionwithkey">>, [RawTx, [PrivKey]], Data),
    SignedTx = result(Res),
    Complete = maps:get(<<"complete">>, SignedTx), true = Complete,
    Hex = maps:get(<<"hex">>, SignedTx),
    {ok, Hex, Data2}
  catch E:R ->
    {error, {E, R}}
  end.

-spec sendrawtransaction(binary(), data()) -> {ok, binary(), data()} | {error, term()}.
sendrawtransaction(Hex, Data) ->
  try
    Wallet = wallet(Data),
    {ok, Res, Data2} = request(<<"/wallet/", Wallet/binary>>, <<"sendrawtransaction">>, [Hex], Data),
    {ok, result(Res), Data2}
  catch E:R ->
    {error, {E, R}}
  end.

-spec result(map()) -> binary().
result(Response) ->
  maps:get(<<"result">>, Response).

-spec block(map()) -> block().
block(Obj) ->
  HexHash = maps:get(<<"hash">>, Obj), true = is_binary(HexHash),
  Height = maps:get(<<"height">>, Obj), true = is_integer(Height),
  HexPrevHash = maps:get(<<"previousblockhash">>, Obj, null), true = is_binary(HexPrevHash),

  %% TODO: To analyze the size field;
  FilteredTxs = lists:filter(fun (Tx) -> is_nulldata(Tx) end, maps:get(<<"tx">>, Obj)),
  Txs = [tx(Tx)||Tx <- FilteredTxs],

  Hash = aeconnector:from_hex(HexHash), PrevHash = aeconnector:from_hex(HexPrevHash),
  aeconnector_block:block(Height, Hash, PrevHash, Txs).

-spec is_nulldata(map()) -> boolean().
is_nulldata(Obj) ->
  Outputs = maps:get(<<"vout">>, Obj),
  Res = [Output || Output = #{ <<"scriptPubKey">> := #{ <<"type">> := T} } <- Outputs, T == <<"nulldata">>],
  Res /= [].

%%-spec is_template(map()) -> boolean().
%%is_template(#{ <<"vin">> := [#{ <<"txinwitness">> := [_, _Pub]}] }) ->
%%  true;
%%is_template(_) ->
%%  false.

%%-spec account(map()) -> binary().
%%account(#{ <<"vin">> := [#{ <<"txinwitness">> := [_, Res]}] }) ->
%%  Res.

%%-spec payload(map()) -> binary().
%%payload(Obj) ->
%%  Outputs = maps:get(<<"vout">>, Obj),
%%  [Res] = [Hex || #{ <<"scriptPubKey">> := #{ <<"hex">> := Hex, <<"type">> := T} } <- Outputs, T == <<"nulldata">>],
%%  Res.

-spec tx(map()) -> tx().
tx(_Obj) ->
%%  PublicKey = account(Obj),
%%  Payload = payload(Obj),
  HexPubKey = <<"0014c0be2b090aab44c79d0883c8f3bc5d32afbcc9a7">>,
  <<"6a", HexPayload/binary>>= <<"6a6b685f324a7a4857345a5842314346435a77344a465237535159524246357851357857415172615035486f357434384b4d50554b6e">>,

  PubKey = aeconnector:from_hex(HexPubKey), Payload = aeconnector:from_hex(HexPayload),
  aeconnector_tx:test_tx(PubKey, Payload).

%%%===================================================================
%%%  Report preparation
%%%===================================================================
-spec compile(data()) -> {ok, atom()}.
compile(Data) ->
  File = template(Data),
  aeconnector_template:compile(File, btc_full_node_telegram).

-spec report(data(), boolean()) -> ok.
report(Data, Connected) ->
  Url = webhook(Data), Headers = [], ContentType = "application/json",

  Height = height(Data),
  Address = address(Data),
  Balance = aeconnector:amount(balance(Data)),
  Hash = hash(Data),
  Info =
    [
      {transaction, <<"a663301f6fa2832df8740a688e0db61f04d20a5e66ac655b59226c1870f1a885">>},
      {amount, <<"0.0089">>},
      {fee, <<"0.0001">>},
      {confirmations, 29029}
    ],
  Vars = aeconnector_template:vars(Height, Address, Balance, Hash, Connected, Info),
  {ok, IoList} = aeconnector_template:render(btc_full_node_telegram, Vars),
  Report = iolist_to_binary(IoList),
  aeconnector_report:post(Url, Headers, ContentType, Report).