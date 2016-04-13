-module(worker_SUITE).

-include_lib("fyler_worker/include/fyler.hrl").
-include_lib("fyler_worker/include/log.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(APP, fyler_worker).
-define(TIMEOUT, 20000).

-compile(export_all).

init_per_suite(Config) ->
	ulitos_app:set_var(fyler_worker, handlers, [{do_nothing, do_nothing}]),
	fyler_worker:start(),
  Config.

end_per_suite(_) ->
  fyler_worker:stop(),
  application:stop(lager),
  ok.

init_per_group(exec, Config) ->
	filelib:ensure_dir("input/"),
	filelib:ensure_dir("output/"),
	Task = #task{
		name = "test", 
		extension = "txt", 
		source = #{type => local, file => <<"input/test.txt">>},
		output = #{type => local, dir => <<"output">>},
		options  = #{key => 1},
		local_dir = "test_dir"
	},
	os:cmd("echo 1 > input/test.txt"),
	[{task, Task}|Config];

init_per_group(_Group, Config) ->
	Config.

end_per_group(exec, Config) ->
	#task{local_dir = LDir} = proplists:get_value(task, Config),
	ulitos_file:recursively_del_dir(LDir),
	ulitos_file:recursively_del_dir("input"),
	ulitos_file:recursively_del_dir("output"),
	ok;

end_per_group(_Group, _Config) ->
	ok.

all() ->
	[
		{group, exec}
	].

groups() ->
	[
		{
			exec, [sequence],
			[
				do_nothing,
				unknown_handler,
				pipe
			]
		}
	].

group(exec) ->
	[{timetrap, ?TIMEOUT}];

group(_Group) ->
	[].

do_nothing(Config) ->
	Task = proplists:get_value(task, Config),
	test_listener:start_link(),
	fyler_fsm:run(Task#task{type = do_nothing}),
	Result = 
		receive 
			Smth -> Smth
		after ?TIMEOUT ->
			timeout
		end,
	?assertMatch({completed, #task{}}, Result),
	{completed, #task{result = TaskResult}} = Result,
	?assertMatch(#{key := 1}, TaskResult),
	ok.

unknown_handler(Config) ->
	Task = proplists:get_value(task, Config),
	test_listener:start_link(),
	fyler_fsm:run(Task#task{type = unknown_handler}),
	Result = 
		receive 
			Smth -> Smth
		after ?TIMEOUT ->
			timeout
		end,
	?assertMatch(error, Result),
	ok.

pipe(Config) ->
	Task = proplists:get_value(task, Config),
	test_listener:start_link(),
	Options = [
		#{type => do_nothing, options  => #{key => 1}}, 
		#{type => do_nothing, options  => #{key => 2}}
	],
	fyler_fsm:run(Task#task{type = pipe, options = Options}),
	Result = 
		receive 
			Smth -> Smth
		after ?TIMEOUT ->
			timeout
		end,
	?assertMatch({completed, #task{}}, Result),
	{completed, #task{result = TaskResult}} = Result,
	?assertMatch([_, _], TaskResult),
	ok.
