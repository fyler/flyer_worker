-module(fyler_worker_server).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-include("../include/log.hrl").
-include("../include/handlers.hrl").
-include("../include/fyler.hrl").

-record(state, {
    stats = #job_stats{} :: #job_stats{}
}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, run_task/1]).

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

run_task(Task) ->
    gen_fsm:send_event(?SERVER, Task).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    self() ! start,
    {ok, start, #state{}}.

idle(#task{} = Task, State) ->
    fyler_worker_event:task_started(Task),
    fyler_downloader:download(Task),
    {next_state, download, State#state{stats = job_stats(Task)}};

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

handle_info(start, start, State) ->
    Category = ?Config(category, undefined),
    {Module, Params} = ?Config(task_source,  #{}),
    ulitos_app:ensure_loaded(?Handlers),
    fyler_worker_sup:start_task_source(Module, Params#{category => Category}),
    {next_state, idle, State};

handle_info({download, ok, #task{file = #file{size = Size}} = Task, Time}, download, #state{stats = Stats} = State) ->
    fyler_processor:process(Task),
    {next_state, process, State#state{stats = Stats#job_stats{download_time = Time, file_size = Size}}};

handle_info({download, failed, Task}, download, #state{stats = Stats} = State) ->
    cleanup_task_files(Task),
    fyler_worker_event:task_failed(Task, Stats#job_stats{status = failed}),
    {next_state, idle, State};

handle_info({process, ok, Task, #job_stats{time_spent = Spent, result_path = Path}}, process, #state{stats = Stats} = State) ->
    fyler_uploader:upload(Task),
    {next_state, upload, State#state{stats = Stats#job_stats{time_spent = Spent, result_path = Path}}};

handle_info({process, failed, Task, Reason}, process, #state{stats = Stats} = State) ->
    cleanup_task_files(Task),
    fyler_worker_event:task_failed(Task, Stats#job_stats{status = failed, error_msg = Reason}),
    {next_state, idle, State};

handle_info({upload, ok, Task, Time}, upload, #state{stats = Stats} = State) ->
    cleanup_task_files(Task),
    fyler_worker_event:task_completed(Task, Stats#job_stats{upload_time = Time}),
    {next_state, idle, State};

handle_info({upload, failed, Task}, upload, #state{stats = Stats} = State) ->
    cleanup_task_files(Task),
    fyler_worker_event:task_failed(Task, Stats#job_stats{status = failed}),
    {next_state, idle, State};
    
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

cleanup_task_files(#task{file = #file{dir = Dir}})->
    ulitos_file:recursively_del_dir(Dir).

job_stats(#task{id = Id, type = Type, file = #file{url = Path}, priority = Priority}) ->
    #job_stats{id = Id, task_type = Type, file_path = Path, priority = Priority}.
