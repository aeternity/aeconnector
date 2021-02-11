%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2021, Aeternity Anstalt
%%% @doc

%%% @end
-module(aeconnector_report).

%% API
-export([get/2, post/4]).
-export([write/3]).

-spec get(list(), [tuple()]) -> {ok, term()}.
get(Url, Headers) ->
  Request = {Url, Headers},
  {ok, _} = httpc:request(get, Request, [], []).

-spec post(list(), [tuple()], list(), binary()) -> {ok, term()}.
post(Url, Headers, ContentType, Report) ->
  Request = {Url, Headers, ContentType, Report},
  {ok, _} = httpc:request(post, Request, [], []).

-spec write(list(), list(), binary()) -> ok.
write(Path, File, Report) ->
  ok = file:write_file(filename:join(Path, File), Report).
