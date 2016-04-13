-export([run/1, validate/1, build_command/1, verify_result/2]).

run(Task) ->
  fyler_cmd:start_link(?MODULE, Task).
