-module(fyler_file_local).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include_lib("fyler_worker/include/log.hrl").
-include_lib("fyler_worker/include/fyler.hrl").
-include_lib("kernel/include/file.hrl").

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
    {ok, state}.

handle_call(abort, _From, State) ->
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({download, Task}, State) ->
  #task{source = #{file := File}, stats = Stats} = Task, 
  #task{name = Name, extension = Ext, local_dir = LDir} = Task,
  From = binary_to_list(File),
  To = filename:join(LDir, [Name, ".", Ext]),
  case file:copy(From, To) of
    {ok, Size} -> 
      NewSource = #{type => local, file => To},
      NewTask = Task#task{stats = Stats#{download_time => 0}, source = NewSource, size = Size},
      fyler_file_manager:complete(NewTask);
    {error, Reason} -> 
      fyler_file_manager:error(Task, Reason)
  end,
  {noreply, State};

handle_cast({upload, Task}, State) ->
  #task{output = #{dir := Dir}, local_dir = LDir, stats = Stats} = Task,
  To = binary_to_list(Dir),
  Cmd = lists:flatten(["cp -r ", LDir, "/. ", To]),
  os:cmd(Cmd),
  NewTask = Task#task{stats = Stats#{upload_time => 0}},
  fyler_file_manager:complete(NewTask),
  {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

  

