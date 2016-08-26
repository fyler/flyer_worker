-module(fyler_task_manager).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include_lib("fyler_worker/include/log.hrl").
-include_lib("fyler_worker/include/fyler.hrl").

-record(state, {
  source :: atom()
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, run_task/1, abort/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

run_task(Task) ->
  gen_server:call(?SERVER, {run_task, Task}).

abort(Id) ->
  gen_server:call(?SERVER, {abort, Id}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Opts) ->

  proc_lib:init_ack({ok, self()}),

  Source = 
    case ?Config(task_source, undefined) of
      undefined -> 
        undefined;
      Module ->
        fyler_worker_sup:start_task_source(Module),
        Module
    end,

  gen_server:enter_loop(?MODULE, [], #state{source = Source}).

handle_call({run_task, Task}, _From, State) ->
  fyler_fsm:run_task(Task),
  {reply, ok, State};

handle_call({abort, Id}, _From, State) ->
  fyler_fsm:abort(Id),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

