%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2021, Aeternity Anstalt
%%% @doc

%%% @end
-module(aeconnector_template).

-export([compile/2]).
-export([render/2]).

-export([vars/6]).

-type file() :: list().
-type template() :: atom().

-export_type([template/0]).

-spec vars(integer(), binary(), binary(), binary(), boolean(), [term()]) -> map().
vars(Height, Address, Balance, Hash, Connected, Info) when is_integer(Height),
                                                           is_binary(Address),
                                                           is_binary(Balance),
                                                           is_binary(Hash),
                                                           is_boolean(Connected),
                                                           is_list(Info) ->
  %% TODO Info may contain initial balance and diff, commitment inclusion etc
  Args =
    [
      {height, Height},
      {address, Address},
      {balance, Balance},
      {hash, Hash},
      {connected, Connected}
    ],
  lists:append(Args, Info).


-spec compile(file(), template()) -> {ok, atom()}.
compile(File, Template) ->
  {ok, _} = erlydtl:compile_file(File, Template).

-spec render(template(), [term()]) -> {ok, binary()}.
render(Template, Vars) ->
  {ok, _IoList} = Template:render(Vars).
