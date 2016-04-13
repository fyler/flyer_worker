-module(fyler_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_status_publisher/1, start_task_source/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_status_publisher(Module) ->
  Spec = {Module, {Module, start_link, []}, permanent, 5000, worker, [Module]},
  supervisor:start_child(fyler_worker_sup, Spec).

start_task_source(Module) ->
  Spec = {Module, {Module, start_link, []}, permanent, 5000, worker, [Module]},
  supervisor:start_child(fyler_worker_sup, Spec).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([fyler_general_sup]) ->
  Children = [
    ?CHILD(fyler_file_s3, worker),
    ?CHILD(fyler_file_local, worker),
    ?CHILD(fyler_file_manager, worker),
    ?CHILD(fyler_exec, worker),
    ?CHILD(fyler_fsm, worker)
  ],
  {ok, {{one_for_all, 5, 10}, Children}};

init([]) ->
  Children = [
    {fyler_general_sup,
      {
        supervisor,
        start_link,
        [{local, fyler_general_sup}, ?MODULE, [fyler_general_sup]]
      },
      permanent,
      infinity,
      supervisor,
      []
    },
    ?CHILD(fyler_worker_event, worker),
    ?CHILD(fyler_worker_event_listener, worker),
    ?CHILD(fyler_task_amqp, worker),
    ?CHILD(fyler_status_publisher, worker),
    ?CHILD(fyler_task_manager, worker)
  ],
  {ok, {{one_for_one, 5, 10}, Children}}.

