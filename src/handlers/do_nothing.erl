-module(do_nothing).

-behaviour(fyler_cmd).
-include_lib("fyler_worker/include/fyler_cmd.hrl").
-include_lib("fyler_worker/include/fyler.hrl").

validate(_) ->
	ok.

build_command(#task{name = Name, extension = Extension, local_dir = LDir, source = #{file := File}}) ->
  NewFile = filename:join(LDir, [Name, "_copy.", Extension]),
	fyler_cmd:format("cp ~s ~s", [File, NewFile]).

verify_result(#task{name = Name, extension = Extension}, _) ->
	{ok, fyler_cmd:format("~s_copy.~s", [Name, Extension])}.