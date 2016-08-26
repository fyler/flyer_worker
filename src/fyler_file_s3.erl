-module(fyler_file_s3).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include_lib("fyler_worker/include/log.hrl").
-include_lib("fyler_worker/include/fyler.hrl").
-include_lib("kernel/include/file.hrl").

-record(state, {
  task = #task{} :: #task{},
  worker = 0 :: non_neg_integer(),
  status = idle :: atom(),
  check = "" :: string(),
  ts = 0 :: non_neg_integer()
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, download/1, upload/1, abort/0]).

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
  gen_server:cast(?SERVER, {download, Task}).

upload(Task) -> 
  gen_server:cast(?SERVER, {upload, Task}).

abort() ->
  gen_server:call(?SERVER, abort).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{}}.

handle_call(abort, _From, #state{worker = Worker, status = download} = State) ->
  aws_cli:abort(Worker),  
  {reply, ok, State#state{status = idle}};

handle_call(abort, _From, #state{worker = Worker, status = upload} = State) ->
  aws_cli:abort(Worker),  
  {reply, ok, State#state{status = idle}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({download, Task}, #state{} = State) ->
  #task{source = #{bucket := Bucket, prefix := Prefix, credentials := Credentials}} = Task, 
  #task{name = Name, extension = Ext, local_dir = LDir} = Task,
  From = binary_to_list(<<"s3://", Bucket/binary, "/", Prefix/binary>>),
  To = [LDir, "/", Name, ".", Ext],
  Worker = aws_cli_s3:cp(From, To, #{credentials => Credentials}),
  {noreply, State#state{task = Task, worker = Worker, status = download, check = To, ts = ulitos:timestamp()}};

handle_cast({upload, Task}, #state{} = State) ->
  #task{output = #{bucket := Bucket, prefix := Prefix, credentials := Credentials}, local_dir = LDir} = Task,
  From = [LDir, "/."],
  To = binary_to_list(<<"s3://", Bucket/binary, "/", Prefix/binary>>),
  Worker = aws_cli_s3:cp_r(From, To, #{credentials => Credentials}),
  {noreply, State#state{task = Task, worker = Worker, status = upload, check = To, ts = ulitos:timestamp()}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({aws_cli_complete, Res}, #state{task = Task, check = Check, status = download, ts = Ts} = State) ->
  #task{stats = Stats} = Task,
  case file:read_file_info(Check) of
    {ok, #file_info{size = Size}} -> 
      Time = ulitos:timestamp() - Ts,
      NewSource = #{type => local, file => Check},
      NewTask = Task#task{stats = Stats#{download_time => Time}, source = NewSource, size = Size},
      fyler_file_manager:complete(NewTask);
    _ -> 
      ?E({aws_sync_error, Res}),
      fyler_file_manager:error(Task, Res)
  end,
  {noreply, State#state{status = idle}};

handle_info({aws_cli_complete, Res}, #state{task = Task, status = upload, ts = Ts} = State) ->
  #task{output = #{bucket := Bucket, prefix := Prefix}, result = Result, stats = Stats} = Task,
  case aws_cli_s3:exist(Bucket, Prefix) of
    true ->
      Time = ulitos:timestamp() - Ts,
      NewResult = format_results(Bucket, Prefix, Result), 
      NewTask = Task#task{stats = Stats#{upload_time => Time}, result = NewResult},
      fyler_file_manager:complete(NewTask);
    _ -> 
      ?E({aws_sync_error, Res}),
      fyler_file_manager:error(Task, Res)
  end,
  {noreply, State#state{status = idle}};

handle_info({aws_cli_error, Res}, #state{task = Task, status = Status} = State) when Status /= idle ->
  fyler_file_manager:error(Task, Res),
  {noreply, State#state{status = idle}};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%internal functions 

format_result(Bucket, Prefix, #{key := Key, type := local, filename := Filename}) ->
  BFilename = list_to_binary(Filename),
  #{key => Key, type => s3, bucket => Bucket, prefix => <<Prefix/binary, "/", BFilename/binary>>}.

format_results(Bucket, Prefix, #{} = Result) ->
  [format_result(Bucket, Prefix, Result)];

format_results(Bucket, Prefix, Results) ->
  Formatter = fun(Result) -> format_result(Bucket, Prefix, Result) end,
  lists:map(Formatter, Results).

%%tests

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

format_test() ->
  Bucket = <<"bucket">>,
  Prefix = <<"test/test">>,
  Results = [
    #{key => 1, type => local, filename => "1.txt"},
    #{key => 2, type => local, filename => "2.txt"}
  ],
  Result1 = format_results(Bucket, Prefix, hd(Results)),
  Result2 = format_results(Bucket, Prefix, Results),
  ?assertMatch([#{key := 1, type := s3, bucket := <<"bucket">>, prefix := <<"test/test/1.txt">>}], Result1),
  [_, Result2_] = Result2,
  ?assertMatch(#{key := 2, type := s3, bucket := <<"bucket">>, prefix := <<"test/test/2.txt">>}, Result2_).

-endif.

  

