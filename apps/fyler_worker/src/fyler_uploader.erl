-module(fyler_uploader).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(AWS_URL(B,F), "s3://"++filename:join(B,F)++"/").

-include("../include/log.hrl").
-include("../include/fyler.hrl").

-record(state, {
	tasks = queue:new() :: queue:queue({#task{}, atom() | pid()})
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, upload/1, upload/2]).

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

upload(Task) -> 
	upload(Task, fyler_worker_server).

upload(Task, Handler) -> 
	gen_server:cast(?SERVER, {upload, Task, Handler}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({upload, Task, Handler}, #state{tasks = Tasks} = State) ->
	self() ! upload,
    {noreply, State#state{tasks = queue:in({Task, Handler}, Tasks)}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(upload, #state{tasks = Tasks} = State) ->
	case queue:is_empty(Tasks) of
		true ->
			{noreply, State};
		false ->
			{Task, Handler} = queue:get(Tasks),
			Time = do_upload(Task),
			case Time of 
				undefined ->
					Handler ! {upload, failed, Task};
				_ -> 
					Handler ! {upload, ok, Task, Time}
			end,
			self() ! upload,
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

do_upload(#task{file = #file{is_aws = true, bucket = Bucket, dir = Dir, target_dir = TargetDir}, acl = Acl}) ->
	AWSPath = ?AWS_URL(Bucket, TargetDir),
	case filelib:is_dir(Dir) of
		true -> 
			Start = ulitos:timestamp(),
			?D({start_upload, AWSPath}),
			Res = aws_cli:copy_folder(Dir, AWSPath, Acl),
			UpTime = ulitos:timestamp() - Start,
			?D({upload_complete, UpTime}),
			case aws_cli:dir_exists(AWSPath) of
				true ->
					UpTime;
				false -> 
					?E({aws_sync_error, Res}),
					undefined
			end;
		_ -> 
			?E({dir_not_found, Dir}),
			undefined
	end.