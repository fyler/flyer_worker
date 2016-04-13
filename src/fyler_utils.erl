-module(fyler_utils).
-include_lib("fyler_worker/include/fyler.hrl").
-export([decode_task/1]).

decode_task(Task) ->
  M = decode(jsx:decode(Task, [return_maps])),
  #{id := Id, type := Type, category := Category, source := Source} = M,
  #{output := Output, name := Name, extension := Extension, options := Options} = M,
  LocalDir = lists:flatten(io_lib:format("~p_~s", [erlang:unique_integer([positive]), binary_to_list(Name)])),
  T = #task{id = Id, type = Type, category = Category, source = Source, output = Output, local_dir = LocalDir},
  T#task{name = binary_to_list(Name), extension = binary_to_list(Extension), options = Options}.

decode(Map) ->
  maps:from_list([decode(Key, Value) || {Key, Value} <- maps:to_list(Map)]).

decode(Key, Value) when is_map(Value) ->
  {binary_to_atom(Key, utf8), decode(Value)};

decode(<<"type">>, Value) ->
  {type, binary_to_atom(Value, utf8)};

decode(<<"category">>, Value) ->
  {category, binary_to_atom(Value, utf8)};

decode(Key, Value) ->
  {binary_to_atom(Key, utf8), Value}.

%%tests

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

decode_test() ->
  ?assertEqual(#{type => video}, decode(#{<<"type">> => <<"video">>})),
  ?assertEqual(#{source => #{type => video}}, decode(#{<<"source">> => #{<<"type">> => <<"video">>}})),
  ?assertEqual(#{category => video}, decode(#{<<"category">> => <<"video">>})),
  ?assertEqual(#{name => <<"1">>}, decode(#{<<"name">> => <<"1">>})).

decode_task_test() ->
  ulitos_app:ensure_started(jsx),
  JSONTask = <<"{
    \"id\": 1,
    \"type\": \"video\",
    \"category\": \"video\",
    \"source\": {
      \"type\": \"s3\",
      \"bucket\": \"test\",
      \"prefix\": \"my/videos/1.flv\",
      \"credentials\": {}
    },
    \"output\": {
      \"type\": \"s3\",
      \"bucket\": \"test\",
      \"prefix\": \"my/videos\",
      \"credentials\": {}
    },
    \"name\": \"1\",
    \"extension\": \"flv\",
    \"options\": {
      \"key\": \"converted\",
      \"audio\": \"-c:a copy\",
      \"video\": \"-c:v copy\",
      \"format\": \"mp4\"
    },
    \"timeout\": 3600
  }">>,
  Task = decode_task(JSONTask),
  ?assertMatch(#task{}, Task),
  ?assertEqual(1, Task#task.id),
  ?assertEqual(video, Task#task.type),
  ?assertEqual(video, Task#task.category),
  ?assertEqual("1", Task#task.name),
  ?assertEqual("flv", Task#task.extension),
  ?assertMatch(#{type := s3}, Task#task.source),
  ?assertMatch(#{type := s3}, Task#task.output),
  ?assertMatch(#{audio := <<"-c:a copy">>}, Task#task.options).

-endif.
