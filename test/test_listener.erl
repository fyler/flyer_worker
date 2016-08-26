%% Copyright
-module(test_listener).
-behaviour(gen_event).
-include_lib("fyler_worker/include/fyler.hrl").

-record(state, {
  handler :: undefined | pid()
}).

%% API
-export([listen/1, start_link/0]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
  terminate/2]).

start_link() ->
  Pid = spawn_link(?MODULE, listen, [self()]),
  {ok, Pid}.

listen(Handler) ->
  fyler_worker_event:add_handler(?MODULE, Handler),
  receive
    _Msg -> ok
  end.

init(Handler) ->
  {ok, #state{handler = Handler}}.

handle_event(#fevent{type = completed, task = Task}, #state{handler = Handler} = State) ->
  Handler ! {completed, Task},
  {ok, State};

handle_event(#fevent{type = error}, #state{handler = Handler} = State) ->
  Handler ! error,
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