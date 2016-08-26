-module(fyler_file_manager).

-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-include_lib("fyler_worker/include/log.hrl").
-include_lib("fyler_worker/include/fyler.hrl").

-record(state, {
    module :: atom(),
    id = 0 :: non_neg_integer()
}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([download/1, upload/1, abort/0, complete/1, error/2]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

-export([idle/2, idle/3, download/2, download/3, upload/2, upload/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

download(Task) ->
  gen_fsm:send_event(?SERVER, {download, Task}).

upload(Task) ->
  gen_fsm:send_event(?SERVER, {upload, Task}).

abort() ->
  gen_fsm:send_event(?SERVER, abort).

complete(Task) ->
  gen_fsm:send_event(?SERVER, {complete, Task}).

error(Task, Reason) ->
  gen_fsm:send_event(?SERVER, {error, Task, Reason}).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  {ok, idle, #state{}}.

idle({download, #task{id = Id, source = #{type := s3}} = Task}, #state{} = State) ->
  fyler_file_s3:download(Task),
  {next_state, download, State#state{id = Id, module = fyler_file_s3}};

idle({download, #task{id = Id, source = #{type := local}} = Task}, #state{} = State) ->
  fyler_file_local:download(Task),
  {next_state, download, State#state{id = Id, module = fyler_file_local}};

idle({upload, #task{id = Id, output = #{type := s3}} = Task}, #state{} = State) ->
  fyler_file_s3:upload(Task),
  {next_state, upload, State#state{id = Id, module = fyler_file_s3}};

idle({upload, #task{id = Id, output = #{type := local}} = Task}, #state{} = State) ->
  fyler_file_local:upload(Task),
  {next_state, upload, State#state{id = Id, module = fyler_file_local}};

idle({download, Task}, State) ->
  fyler_fsm ! {download, error, Task, "Unknown source"},
  {next_state, idle, State};

idle({upload, Task}, State) ->
  fyler_fsm ! {upload, error, Task, "Unknown source"},
  {next_state, idle, State};

idle(_Event, State) ->
  {next_state, idle, State}.

idle(_Event, _From, State) ->
  {reply, ok, idle, State}.

download(abort, #state{module = Module} = State) ->
  Module:abort(),
  {next_state, idle, State};

download({complete, #task{id = Id} = Task}, #state{id = Id} = State) ->
  fyler_fsm ! {download, ok, Task},
  {next_state, idle, State};

download({error, #task{id = Id} = Task, Reason}, #state{id = Id} = State) ->
  fyler_fsm ! {download, error, Task, Reason},
  {next_state, idle, State};

download(_Event, State) ->
  {next_state, download, State}.

download(_Event, _From, State) ->
  {reply, ok, download, State}.

upload(abort, #state{module = Module} = State) ->
  Module:abort(),
  {next_state, idle, State};

upload({complete, #task{id = Id} = Task}, #state{id = Id} = State) ->
  fyler_fsm ! {upload, ok, Task},
  {next_state, idle, State};

upload({error, #task{id = Id} = Task, Reason}, #state{id = Id} = State) ->
  fyler_fsm ! {upload, error, Task, Reason},
  {next_state, idle, State};

upload(_Event, State) ->
  {next_state, upload, State}.

upload(_Event, _From, State) ->
  {reply, ok, upload, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------




