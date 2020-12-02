%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc

%%% @end
-module(aeconnector_schedule).

-export([address/1]).
-export([amount/1]).
-export([fee/1]).
-export([comment/1]).

-export([from_file/1]).
-export([item/4]).

-export([is_item/1]).

-record(item, { address::binary(), amount::float(), fee::float(), comment::binary() }).

-type item() :: #item{}.

-export_type([item/0]).

-spec from_file(list()) -> [item()].
from_file(Path) ->
  Opt = [{str_node_as_binary, true}, {map_node_format, map}],
  yamerl_constr:file(Path, Opt).

-spec item(binary(), float(), float(), binary()) -> item().
item(Address, Amount, Fee, Comment) when
  is_binary(Address), is_float(Amount), is_float(Fee) ->
  #item{ address = Address, amount = Amount, fee = Fee, comment = Comment }.

-spec address(item()) -> binary().
address(Item) ->
  Item#item.address.

-spec amount(item()) -> float().
amount(Item) ->
  Item#item.amount.

-spec fee(item()) -> float().
fee(Item) ->
  Item#item.fee.

-spec comment(item()) -> binary().
comment(Item) ->
  Item#item.comment.

-spec is_item(term()) -> boolean().
is_item(#item{}) ->
  true;
is_item(_) ->
  false.
