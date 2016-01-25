-module(fyler_downloader).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include("../include/log.hrl").
-include("../include/fyler.hrl").
-include_lib("kernel/include/file.hrl").

-record(state, {
	tasks = queue:new() :: queue:queue({#task{}, atom() | pid()})
}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, download/1, download/2]).

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

download(Task) -> 
	download(Task, fyler_worker_server).

download(Task, Handler) -> 
	gen_server:cast(?SERVER, {download, Task, Handler}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({download, Task, Handler}, #state{tasks = Tasks} = State) ->
	self() ! download,
    {noreply, State#state{tasks = queue:in({Task, Handler}, Tasks)}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(download, #state{tasks = Tasks} = State) ->
	case queue:is_empty(Tasks) of
		true ->
			{noreply, State};
		false ->
			{#task{file = File} = Task, Handler} = queue:get(Tasks),
			{Time, Size} = do_download(Task),
			NewTask = Task#task{file = File#file{size = Size}},
			case Time of 
				undefined ->
					Handler ! {download, failed, Task};
				_ -> 
					Handler ! {download, ok, NewTask, Time}
			end,
			self() ! download,
			{noreply, State#state{tasks = queue:drop(Tasks)}}
	end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_download(#task{file = #file{url = Path, tmp_path = Tmp}}) ->
	Start = ulitos:timestamp(),
	?D({copy_from_aws_to, Tmp}),
	Res = aws_cli:copy_object("s3://" ++ Path, Tmp),
	case file:read_file_info(Tmp) of
    	{ok, #file_info{size = Size2}} -> 
    		DTime = ulitos:timestamp() - Start,
			{DTime, Size2};
		_ -> 
			?E({aws_sync_error, Res}),
			{undefined, undefined}
	end.

	

