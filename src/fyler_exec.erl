-module(fyler_exec).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include_lib("fyler_worker/include/log.hrl").
-include_lib("fyler_worker/include/fyler.hrl").

-record(state, {
  module :: atom(),  
  worker :: undefined | pid()
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, run/1, abort/0]).

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

run(Task) ->
  gen_server:cast(?MODULE, {run, Task}).

abort() ->
  gen_server:call(?MODULE, abort).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  {ok, #state{}}.

handle_call(abort, _From, #state{module = Module, worker = Pid} = State) ->
  Module:abort(Pid),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({run, #task{type = pipe} = Task}, #state{} = State) ->
  {ok, Pid} = fyler_pipe:run(Task),
  {noreply, State#state{module = fyler_pipe, worker = Pid}};

handle_cast({run, #task{type = Type} = Task}, #state{} = State) ->
  case proplists:get_value(Type, ?Config(handlers, [])) of
    undefined ->
      fyler_fsm ! {process, error, Task, "No handler"},
      {noreply, State};
    Module ->
      {ok, Pid} = Module:run(Task),
      {noreply, State#state{module = Module, worker = Pid}}
  end;

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({ok, Task}, State) ->
  fyler_fsm ! {process, ok, Task},
  {noreply, State};

handle_info({error, Task, Reason}, State) ->
  fyler_fsm ! {process, error, Task, Reason},
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

