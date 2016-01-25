-module(fyler_worker_event).
-behaviour(gen_event).

-include("../include/log.hrl").
-include("../include/fyler.hrl").

%% External API
-export([start_link/0, notify/1, add_handler/2, add_sup_handler/2, remove_handler/1]).
-export([stop_handlers/0]).

%% gen_event callbacks
-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).

-export([task_started/1, task_completed/2, task_failed/2]).

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

%% @doc Send when task job is started.
%% @end

task_started(#task{category = Category} = Task) ->
    notify(#fevent{type = start, node = node(), category = Category, task = Task}).

%% @doc Send when task job is finished successfully.
%% @end

task_completed(#task{category = Category} = Task, Stats) ->
    notify(#fevent{type = complete, node = node(), category = Category, task = Task, stats = Stats}).

%% @doc Send when task job is failed.
%% @end

task_failed(#task{category = Category} = Task, #job_stats{error_msg = Error} = Stats) ->
    notify(#fevent{type = fail, node=node(), category = Category, task = Task, error = Error, stats = Stats}).

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
