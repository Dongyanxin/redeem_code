%%%-------------------------------------------------------------------
%%% @author xin
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 三月 2015 上午9:55
%%%-------------------------------------------------------------------
-module(common).
-author("xin").

%% API
-export([
  startup/0,
  now/0,
  now/1,
  msnow/0,
  debug/1
]).

-include("common.hrl").


startup() ->

  io:format("~n ** redeem code create **~n"),
  application:start(redeem).

% debug点
debug(Message) ->

  case ?IsDEBUG of

    true -> io:format("~n ** DEBUG-INFO:~p **~n", [Message]);

    _ -> ok
  end.


% 当日时间戳
now() ->
  {M, S, _} = os:timestamp(),
  M * 1000000 + S.


% 目标日期 转 秒数
now(DateTime) ->
  calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200 - 8 * 3600.


%% @doc 微秒时间戳
msnow() ->
  {MegaSecs, Secs, MicroSecs} = erlang:now(),
  1000000000 * MegaSecs + Secs * 1000 + MicroSecs div 1000.