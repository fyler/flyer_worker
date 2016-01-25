-module(fyler_processor).
-behaviour(gen_server).

-include("../include/log.hrl").
-include("../include/fyler.hrl").

-record(state, {
    handler :: atom() | pid(),
    task :: #task{}
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2, process/1, process/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Task, Handler) ->
    gen_server:start_link(?MODULE, {Task, Handler}, []).

process(Task) ->
	process(Task, fyler_worker_server).

process(Task, Handler) ->
    fyler_worker_sup:start_processing(Task, Handler).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({Task, Handler}) ->
    process_flag(trap_exit, true),
    self() ! start,
    {ok, #state{handler = Handler, task = Task}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, #state{task = #task{type = Type, file = File, options = Opts}} = State) ->
    Self = self(),
    case erlang:function_exported(Type, run, 2) of
        true ->
            ?D({start_processing, File#file.name}),
            spawn_link(
                fun() ->
                  case erlang:apply(Type, run, [File, Opts]) of
                    {ok, Stats} -> Self ! {process_complete, Stats};
                    {error, Reason} -> Self ! {error, Reason}
                  end
                end
            );
        false -> 
            self() ! {error, handler_not_found},
            undefined
    end,
    {noreply, State};

handle_info({process_complete, Stats}, #state{handler = Handler, task = Task} = State) ->
    Handler ! {process, ok, Task, Stats},
    {noreply, State};

handle_info({error, Reason}, #state{handler = Handler, task = Task} = State) ->
    Handler ! {process, failed, Task, Reason},
    {noreply, State};

handle_info({'EXIT', _Pid, normal}, State) ->
    {stop, normal, State};

handle_info({'EXIT', _Pid, Reason}, #state{handler = Handler, task = #task{id = Id, file = #file{url = Path, size = Size}, type = Type} = Task} = State) ->
    Stats = #job_stats{id = Id, error_msg = Reason, status = failed, ts = ulitos:timestamp(), task_type = Type, file_path = Path, file_size = Size},
    Handler ! {process, failed, Task, Stats},
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

