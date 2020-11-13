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

%%%===================================================================
%%%  aeconnector behaviour
%%%===================================================================

-spec connect(map(), function()) -> {ok, pid()} | {error, term()}.
connect(Args, Callback) when is_map(Args), is_function(Callback)  ->
  User = maps:get(<<"user">>, Args),
  Password = maps:get(<<"password">>, Args),
  Host = maps:get(<<"host">>, Args, <<"127.0.0.1">>),
  Port = maps:get(<<"port">>, Args, 8332),
  SSL = maps:get(<<"ssl">>, Args, false),
  Timeout = maps:get(<<"timeout">>, Args, 30000),
  ConTimeout = maps:get(<<"connect_timeout">>, Args, 3000),
  AutoRedirect = maps:get(<<"autoredirect">>, Args, true),
  URL = url(Host, Port, SSL),
  Auth = auth(User, Password),
  Serial = 0,
  Seed = erlang:md5(term_to_binary({Serial, node(), make_ref()})),
  Data = #data{
      auth = Auth,
      url = URL,
      serial = Serial,
      seed = Seed,
      callback = Callback,
      timeout = Timeout,
      connect_timeout = ConTimeout,
      autoredirect = AutoRedirect
    },
  gen_statem:start_link({local, ?MODULE}, ?MODULE, Data, []).

-spec send_tx(binary(), binary()) -> ok.
send_tx(Delegate, Commitment) ->
  gen_statem:call(?MODULE, {send_tx, Delegate, Commitment}).

-spec get_top_block() -> {ok, aeconnector:block()} | {error, term()}.
get_top_block() ->
  gen_statem:call(?MODULE, {get_top_block}).

-spec get_block_by_hash(binary()) -> {ok, aeconnector:block()} | {error, term()}.
get_block_by_hash(Hash) ->
  gen_statem:call(?MODULE, {get_block_by_hash, Hash}).

-spec dry_send_tx(binary(), binary()) -> ok.
dry_send_tx(Delegate, Commitment) ->
  gen_statem:call(?MODULE, {dry_send_tx, Delegate, Commitment}).

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
    autoredirect::boolean()
  }).

-type data() :: #data{}.

init(Data) ->
  %% TODO: to perform get top and info reqs;
  %% getblockchaininfo, getnetworkinfo, and getwalletinfo
  {ok, connected, Data, []}.

callback_mode() ->
  [state_functions, state_enter].

terminate(_Reason, _State, _Data) ->
  ok.

%%%===================================================================
%%%  State machine callbacks
%%%===================================================================

connected(enter, _OldState, Data) ->
  {keep_state, Data};

connected(call, {send_tx, Delegate, Commitment}, Data) ->
  Seed = seed(Data),
  Rpc = rpc(Method, Params, _Id = base64:encode(Seed)),
  request(Rpc, Data),
  {keep_state, Data, [postpone]}.

confirmed(enter, _OldState, Data) ->
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

-spec url(data()) -> binary().
url(Data) ->
  Data#data.url.

-spec seed(data()) -> binary().
seed(Data) ->
  Data#data.seed.

-spec callback(data()) -> function().
callback(Data) ->
  Data#data.callback.

-spec top(data()) -> binary().
top(Data) ->
  Data#data.top.

-spec top(data(), binary()) -> data().
top(Data, Top) ->
  Data#data{top = Top}.

-spec tx(data()) -> binary().
tx(Data) ->
  Data#data.tx.

-spec tx(data(), binary()) -> data().
tx(Data, Tx) ->
  Data#data{tx = Tx}.

%%%===================================================================
%%%  HTTP protocol
%%%===================================================================

url(Host, Port, true = SSL) when is_list(Host), is_integer(Port) ->
  path("https://", Host, Port);
url(Host, Port, _) when is_list(Host), is_integer(Port) ->
  path("http://", Host, Port).

path(Scheme, Host, Port) ->
  lists:concat([Scheme, Host, ":", Port, "/"]).

-spec request() -> {ok}.
request(Rpc, Data) ->
  try
    Auth = auth(Data),
    Url = url(Data),
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
    {ok, jsx:decode(Res)}
  catch E:R:S ->
    lager:error("Req: ~p, Error: ~p Reason: ~p Stacktrace", [Req, E, R, S]),
    {error, {E, R}}
  end.

%%%===================================================================
%%%  BTC protocol
%%%===================================================================

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