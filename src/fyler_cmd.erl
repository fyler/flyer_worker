-module(fyler_cmd).
-behaviour(gen_server).

-include_lib("fyler_worker/include/log.hrl").
-include_lib("fyler_worker/include/fyler.hrl").

-callback validate(Task :: #task{}) -> ok | {error, Reason :: any()}.
-callback build_command(Task :: #task{}) -> Cmd :: string().
-callback verify_result(Task :: #task{}, Output :: string()) -> {ok, Filename :: string()} | {error, Reason :: any()}.

-record(state, {
  module :: atom(),
  task :: #task{},
  handler :: undefined | pid(),
  pid :: pid(),
  ospid = 0 :: non_neg_integer(), 
  stdout = <<"">> :: binary(),
  ts = 0 :: non_neg_integer()
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2, abort/1]).
-export([format/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Module, Task) ->
  gen_server:start_link(?MODULE, {Module, Task, self()}, []).

abort(Pid) ->
  gen_server:call(Pid, abort).

format(Template, Args) ->
  lists:flatten(io_lib:format(Template, Args)).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({Module, Task, Handler}) ->
  self() ! start,
  {ok, #state{module = Module, task = Task, handler = Handler}}.

handle_call(abort, _From, State) ->
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(start, #state{module = Module, task = Task, handler = Handler} = State) ->
  case Module:validate(Task) of
    ok ->
      Cmd = Module:build_command(Task),
      {ok, Pid, OsPid} = exec:run(Cmd, [stdout, monitor]),
      {noreply, State#state{pid = Pid, ospid = OsPid, ts = ulitos:timestamp()}};
    {error, Reason} ->
      Handler ! {error, Task, Reason},
      {stop, normal, State}
  end;

handle_info({stdout, OsPid, Msg}, #state{ospid = OsPid, stdout = StdOut} = State) ->
  {noreply, State#state{stdout = <<StdOut/binary, Msg/binary>>}};

handle_info({'DOWN', _Ref, process, Pid, _}, #state{module = Module, task = Task, handler = Handler, pid = Pid, stdout = Stdout, ts = Ts} = State) ->
  case Module:verify_result(Task, Stdout) of
    {ok, Filename} ->
      #task{options = #{key := Key}} = Task,
      Result = #{key => Key, type => local, filename => Filename},
      #task{stats = Stats} = Task,
      Handler ! {ok, Task#task{stats = Stats#{process_time => ulitos:timestamp() - Ts}, result = Result}};
    {error, Reason} ->
      Handler ! {error, Task, Reason}
  end,
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

