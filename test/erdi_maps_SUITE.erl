-module(erdi_maps_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([get_top_level/1, get_top_level_missing/1, get_second_level/1, get_third_level/1,
         get_middle_missing/1, get_too_many_keys/1, get_compound_key/1,
         get_too_many_compound_keys/1, is_key_top_level/1, is_not_key_top_level/1,
         is_key_second_level/1, is_not_key_second_level/1, is_key_compound/1, get_hits_list/1]).

all() ->
  [get_top_level,
   get_top_level_missing,
   get_second_level,
   get_third_level,
   get_middle_missing,
   get_too_many_keys,
   get_compound_key,
   get_too_many_compound_keys,
   is_key_top_level,
   is_not_key_top_level,
   is_key_second_level,
   is_not_key_second_level,
   is_key_compound,
   get_hits_list].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

get_top_level(_Config) ->
  Map = #{<<"top">> => <<"level">>, <<"bottom">> => <<"dweller">>},
  ?assertEqual(<<"level">>, erdi_maps:get(<<"top">>, Map)).

get_top_level_missing(_Config) ->
  Map = #{<<"top">> => <<"level">>, <<"bottom">> => <<"dweller">>},
  ?assertEqual(undefined, erdi_maps:get(<<"not_top">>, Map)).

get_second_level(_Config) ->
  Map = #{<<"top">> => #{<<"second">> => <<"level">>}, <<"bottom">> => <<"dweller">>},
  ?assertEqual(<<"level">>, erdi_maps:get([<<"top">>, <<"second">>], Map)).

get_third_level(_Config) ->
  Map =
    #{<<"top">> => #{<<"second">> => #{<<"third">> => <<"level">>}},
      <<"bottom">> => <<"dweller">>},
  ?assertEqual(<<"level">>, erdi_maps:get([<<"top">>, <<"second">>, <<"third">>], Map)).

get_middle_missing(_Config) ->
  Map =
    #{<<"top">> => #{<<"not_second">> => #{<<"third">> => <<"level">>}},
      <<"bottom">> => <<"dweller">>},
  ?assertEqual(undefined, erdi_maps:get([<<"top">>, <<"second">>, <<"third">>], Map)).

get_too_many_keys(_Config) ->
  Map = #{<<"top">> => #{<<"second">> => <<"level">>}, <<"bottom">> => <<"dweller">>},
  ?assertEqual(undefined, erdi_maps:get([<<"top">>, <<"second">>, <<"third">>], Map)).

get_compound_key(_Config) ->
  Map =
    #{<<"top">> => #{<<"second">> => #{<<"third">> => <<"level">>}},
      <<"bottom">> => <<"dweller">>},
  ?assertEqual(<<"level">>, erdi_maps:get(<<"top.second.third">>, Map)).

get_too_many_compound_keys(_Config) ->
  Map = #{<<"top">> => #{<<"second">> => <<"level">>}, <<"bottom">> => <<"dweller">>},
  ?assertEqual(undefined, erdi_maps:get(<<"top.second.third">>, Map)).

is_key_top_level(_Config) ->
  Map =
    #{<<"top">> => #{<<"second">> => #{<<"third">> => <<"level">>}},
      <<"bottom">> => <<"dweller">>},
  ?assertEqual(true, erdi_maps:is_key(<<"top">>, Map)).

is_not_key_top_level(_Config) ->
  Map =
    #{<<"top">> => #{<<"second">> => #{<<"third">> => <<"level">>}},
      <<"bottom">> => <<"dweller">>},
  ?assertEqual(false, erdi_maps:is_key(<<"not_top">>, Map)).

is_key_second_level(_Config) ->
  Map =
    #{<<"top">> => #{<<"second">> => #{<<"third">> => <<"level">>}},
      <<"bottom">> => <<"dweller">>},
  ?assertEqual(true, erdi_maps:is_key([<<"top">>, <<"second">>], Map)).

is_not_key_second_level(_Config) ->
  Map =
    #{<<"top">> => #{<<"second">> => #{<<"third">> => <<"level">>}},
      <<"bottom">> => <<"dweller">>},
  ?assertEqual(false, erdi_maps:is_key([<<"top">>, <<"not_second">>], Map)).

is_key_compound(_Config) ->
  Map =
    #{<<"top">> => #{<<"second">> => #{<<"third">> => <<"level">>}},
      <<"bottom">> => <<"dweller">>},
  ?assertEqual(true, erdi_maps:is_key(<<"top.second">>, Map)).

get_hits_list(_Config) ->
  Map =
    #{<<"top">> => #{<<"second">> => [#{<<"third">> => <<"level">>}]},
      <<"bottom">> => <<"dweller">>},
  ?assertEqual(undefined, erdi_maps:get(<<"top.second.third">>, Map)).
