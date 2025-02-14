%% Copyright 2025, Chris Maguire <cwmaguire@protonmail.com>
-module(erdi_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([connect/1]).

all() ->
  [connect].

init_per_suite(Config) ->
  application:ensure_all_started([erdi_server, erdi]),
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

connect(_Config) ->
  ok.
