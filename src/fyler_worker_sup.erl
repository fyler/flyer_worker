-module(fyler_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_task_source/2, start_processing/2, stop_processing/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_task_source(Module, Opts) ->
	Spec = {fyler_task_source, {Module, start_link, [Opts]}, permanent, 5000, worker, [Module]},
	supervisor:start_child(fyler_worker_sup, Spec).

start_processing(Task, Handler) ->
	supervisor:start_child(fyler_processing, [Task, Handler]).

stop_processing(Pid) ->
	supervisor:terminate_child(fyler_processing, Pid).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([fyler_processing]) ->
	{ok, {{simple_one_for_one, 5, 10}, [
	    {undefined, {fyler_processor, start_link, []},
	      temporary, 2000, worker, [fyler_processor]}
  	]}};

init([]) ->
	Children = [
		{fyler_processing,
			{
				supervisor,
				start_link,
				[{local, fyler_processing}, ?MODULE, [fyler_processing]]
			},
			permanent,
			infinity,
			supervisor,
			[]
		},
		?CHILD(fyler_worker_event, worker),
		?CHILD(fyler_worker_event_listener, worker),
		?CHILD(fyler_uploader, worker),
		?CHILD(fyler_downloader, worker),
		?CHILD(fyler_worker_server, worker)
	],
    {ok, { {one_for_one, 5, 10}, Children} }.

