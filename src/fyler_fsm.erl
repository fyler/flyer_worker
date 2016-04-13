-module(fyler_fsm).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-include_lib("fyler_worker/include/log.hrl").
-include_lib("fyler_worker/include/fyler.hrl").

-record(state, {
  id = 0 :: non_neg_integer(),
  task :: undefined | #task{}
}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, run/1, abort/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3,
     handle_sync_event/4, handle_info/3, terminate/3,
     code_change/4]).

-export([idle/2, idle/3, download/2, download/3,
  upload/2, upload/3, process/2, process/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

run(Task) ->
  gen_fsm:send_event(?SERVER, Task).

abort(Id) ->
  gen_fsm:send_event(?SERVER, {abort, Id}).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  {ok, idle, #state{}}.

idle(#task{id = Id, local_dir = LDir} = Task, #state{} = State) ->
  filelib:ensure_dir(LDir ++ "/"),
  fyler_file_manager:download(Task),
  fyler_worker_event:downloading(Task),
  {next_state, download, State#state{id = Id, task = Task}};

idle(_Event, State) ->
  {next_state, idle, State}.

idle(_Event, _From, State) ->
  {reply, ok, idle, State}.

download(_Event, State) ->
  {next_state, idle, State}.

download(_Event, _From, State) ->
  {reply, ok, idle, State}.

upload(_Event, State) ->
  {next_state, idle, State}.

upload(_Event, _From, State) ->
  {reply, ok, idle, State}.

process(_Event, State) ->
  {next_state, idle, State}.

process(_Event, _From, State) ->
  {reply, ok, idle, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

handle_info({download, ok, #task{id = Id} = Task}, download, #state{id = Id} = State) ->
  fyler_exec:run(Task),
  fyler_worker_event:processing(Task),
  {next_state, process, State};

handle_info({download, error, #task{id = Id} = Task, Reason}, download, #state{id = Id} = State) ->
  fyler_worker_event:error(Task, Reason),
  cleanup_task_files(Task),
  {next_state, idle, State};

handle_info({process, ok, #task{id = Id, source = #{file := File}} = Task}, process, #state{id = Id} = State) ->
  file:delete(File),
  fyler_file_manager:upload(Task),
  fyler_worker_event:uploading(Task),
  {next_state, upload, State};

handle_info({process, error, #task{id = Id} = Task, Reason}, process, #state{id = Id} = State) ->
  fyler_worker_event:error(Task, Reason),
  cleanup_task_files(Task),
  {next_state, idle, State};

handle_info({upload, ok, #task{id = Id} = Task}, upload, #state{id = Id} = State) ->
  fyler_worker_event:completed(Task),
  cleanup_task_files(Task),
  {next_state, idle, State};

handle_info({upload, error, #state{id = Id} = Task, Reason}, upload, #state{id = Id} = State) ->
  fyler_worker_event:error(Task, Reason),
  cleanup_task_files(Task),
  {next_state, idle, State};
  
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(Reason, StateName, #state{task = Task}) when StateName /= idle ->
  fyler_worker_event:error(Task, Reason),
  ok;

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

cleanup_task_files(#task{local_dir = LDir})->
  ulitos_file:recursively_del_dir(LDir).
