-module(aeconnector_tx).

-export([account/1]).
-export([payload/1]).

%% TODO: To test;
-export([test_tx/2]).

-export([tx/2]).

-record(tx, { account::binary(), payload::binary() }).

-type tx() :: #tx{}.

-export_type([tx/0]).

-spec account(tx()) -> binary().
account(Tx) ->
  Tx#tx.account.

-spec payload(tx()) -> binary().
payload(Tx) ->
  Tx#tx.payload.

-spec tx(Account::binary(), Payload::binary()) -> tx().
tx(Account, Payload)
  when is_binary(Account), is_binary(Payload) ->
  #tx{ account = Account, payload = Payload}.

-spec test_tx(Vin::term(), Vout::term()) -> tx().
test_tx(Vin, Vout) ->
  #tx{ account = Vin, payload = Vout}.