-module(fyler_pipe).
-behaviour(gen_server).

-include_lib("fyler_worker/include/log.hrl").
-include_lib("fyler_worker/include/fyler.hrl").

-record(state, {
  task :: #task{},
  options = [] :: [#{}],
  handler :: undefined | pid(),
  worker :: undefined | pid(),
  ts = 0 :: non_neg_integer()
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, run/1, abort/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Task) ->
  gen_server:start_link(?MODULE, {Task, self()}, []).

run(Task) ->
  start_link(Task).

abort(Pid) ->
  gen_server:call(Pid, abort).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({#task{options = Options} = Task, Handler}) ->
  self() ! start,
  {ok, #state{task = Task#task{result = []}, options = Options, handler = Handler}}.

handle_call(abort, _From, State) ->
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(start, #state{task = Task, options = [SubOptions|Options_], handler = Handler} = State) ->
  #task{type = Type} = SubTask = create_task("", Task, SubOptions),
  case proplists:get_value(Type, ?Config(handlers, [])) of
    undefined ->
      Handler ! {error, Task, "No handler"},
      {stop, normal, State};
    Module ->
      {ok, Pid} = Module:run(SubTask),
      {noreply, State#state{options = Options_, worker = Pid}}
  end;

handle_info({ok, SubTask}, #state{task = Task, options = [SubOptions|Options_], handler = Handler} = State) ->
  #task{result = #{filename := Filename} = Result} = SubTask,
  #task{result = FullResult} = Task, 
  #task{type = Type} = NewSubTask = create_task(Filename, Task, SubOptions),
  case proplists:get_value(Type, ?Config(handlers, [])) of
    undefined ->
      Handler ! {error, Task, "No handler"},
      {stop, normal, State};
    Module ->
      {ok, Pid} = Module:run(NewSubTask),
      {noreply, State#state{task = Task#task{result = [Result|FullResult]}, options = Options_, worker = Pid}}
  end;

handle_info({ok, SubTask}, #state{task = #task{stats = Stats} = Task, options = [], handler = Handler, ts = Ts} = State) ->
  #task{result = Result} = SubTask,
  #task{result = FullResult} = Task, 
  Handler ! {ok, Task#task{stats = Stats#{process_time => ulitos:timestamp() - Ts}, result = [Result|FullResult]}},
  {stop, normal, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

create_task("", Task, #{type := Type, options := Options}) ->
  Task#task{type = Type, options = Options};

create_task(Filename, #task{local_dir = LDir} = Task, #{type := Type, options := Options}) ->
  [_|Extension] = filename:extension(Filename),
  Name = string:substr(Filename, 1, length(Filename) - length(Extension) - 1),
  Source = #{type => local, file => filename:join(LDir, Filename)},
  Task#task{type = Type, name = Name, extension = Extension, source = Source, options = Options}.


