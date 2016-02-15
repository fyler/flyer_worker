-module(video_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

init_per_suite(Config) ->
  ulitos_app:set_var(?APP, category, video),
  fyler_worker:start(),
  Config.

end_per_suite(_) ->
  fyler_worker:stop(),
  application:stop(lager),
  ok.