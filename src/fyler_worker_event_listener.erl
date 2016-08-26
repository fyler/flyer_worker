%% Copyright
-module(fyler_worker_event_listener).
-behaviour(gen_event).
-include("../include/log.hrl").
-include("./include/fyler.hrl").

%% API
-export([listen/0, start_link/0]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
  terminate/2]).

start_link() ->
  Pid = spawn_link(?MODULE, listen, []),
  {ok, Pid}.

listen() ->
  fyler_worker_event:add_sup_handler(?MODULE, []),
  receive
    Msg -> ?D({listen, Msg})
  end.

init(_Args) ->
  {ok, []}.

handle_event(#fevent{type = downloading, task = Task}, State) ->
  fyler_status_publisher:publish_status(downloading, Task),
  {ok, State};

handle_event(#fevent{type = processing, task = Task}, State) ->
  fyler_status_publisher:publish_status(processing, Task),
  {ok, State};

handle_event(#fevent{type = uploading, task = Task}, State) ->
  fyler_status_publisher:publish_status(uploading, Task),
  {ok, State};

handle_event(#fevent{type = completed, task = Task}, State) ->
  fyler_status_publisher:publish_status(completed, Task),
  {ok, State};

handle_event(#fevent{type = error, task = Task, error = Error}, State) ->
  fyler_status_publisher:publish_status(error, Task, Error),
  {ok, State};

handle_event(#fevent{type = aborted, task = Task}, State) ->
  fyler_status_publisher:publish_status(aborted, Task),
  {ok, State};

handle_event(_Event, State) ->
  {ok, State}.

handle_call(_, State) ->
  {ok, ok, State}.

handle_info(_, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.