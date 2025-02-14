-module(erdi_maps).

-export([get/2, is_key/2]).

get([], Next) ->
  Next;
get([Key | Keys], Map) when is_binary(Key), is_map(Map) ->
  case maps:get(Key, Map, undefined) of
    undefined ->
      undefined;
    Next ->
      get(Keys, Next)
  end;
get(Key, Map) when is_binary(Key), is_map(Map) ->
  Keys = split_key(Key),
  get(Keys, Map);
get(_Key, _Map) ->
  undefined.

is_key(Key, Map) when is_map(Map) ->
  case get(Key, Map) of
    undefined ->
      false;
    _ ->
      true
  end;
is_key(_Key, _Map) ->
  false.

split_key(Key) when is_binary(Key) ->
  binary:split(Key, <<".">>, [global, trim_all]);
split_key(Key) when is_list(Key) ->
  Key;
split_key(Key) ->
  [Key].
