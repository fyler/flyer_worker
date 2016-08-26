-module(fyler_worker_event).
-behaviour(gen_event).

-include_lib("../include/log.hrl").
-include("../include/fyler.hrl").

%% External API
-export([start_link/0, notify/1, add_handler/2, add_sup_handler/2, remove_handler/1]).
-export([stop_handlers/0]).

%% gen_event callbacks
-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).

-export([downloading/1, processing/1, uploading/1, completed/1, error/2, aborted/1]).

start_link() ->
  gen_event:start_link({local, ?MODULE}).

stop_handlers() ->
  [gen_event:delete_handler(?MODULE, Handler, []) || Handler <- gen_event:which_handlers(?MODULE)].

%% @doc Dispatch event to listener
%% @end

-spec notify(any()) -> ok.

notify(Event) ->
  gen_event:notify(?MODULE, Event).


%% @doc
%% @end

-spec add_handler(any(), [any()]) -> ok.

add_handler(Handler, Args) ->
  gen_event:add_handler(?MODULE, Handler, Args).


%% @doc
%% @end

-spec add_sup_handler(any(), [any()]) -> ok.

add_sup_handler(Handler, Args) ->
  gen_event:add_sup_handler(?MODULE, Handler, Args).

%% @doc
%% @end

-spec remove_handler(any()) -> ok.

remove_handler(Handler) ->
  gen_event:delete_handler(?MODULE, Handler,[]).

%% @doc Send when task is started to download.
%% @end

downloading(Task) ->
  notify(#fevent{type = downloading, task = Task}).

%% @doc Send when task is started to process.
%% @end

processing(Task) ->
  notify(#fevent{type = processing, task = Task}).

%% @doc Send when task is started to upload.
%% @end

uploading(Task) ->
  notify(#fevent{type = uploading, task = Task}).

%% @doc Send when task job is finished successfully.
%% @end

completed(Task) ->
  notify(#fevent{type = completed, task = Task}).

%% @doc Send when task job is failed.
%% @end

error(Task, Error) ->
  notify(#fevent{type = error, task = Task, error = Error}).

%% @doc Send when task job is aborted.
%% @end

aborted(Task) ->
  notify(#fevent{type = aborted, task = Task}).

init([]) ->
  {ok, state}.


%% @private
handle_call(Request, State) ->
  {ok, Request, State}.

%% @private
handle_event(_Event, State) ->
  {ok, State}.


%% @private
handle_info(_Info, State) ->
  {ok, State}.


%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
