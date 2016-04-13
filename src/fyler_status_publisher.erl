-module(fyler_status_publisher).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include_lib("fyler_worker/include/log.hrl").
-include_lib("fyler_worker/include/fyler.hrl").

-record(state, {
  publishers = #{} :: #{atom() => pid()}
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, publish_status/2, publish_status/3, publisher_started/2]).

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

publish_status(Status, Task) ->
  gen_server:cast(?SERVER, {Status, Task}).

publish_status(Status, Task, Msg) ->
  gen_server:cast(?SERVER, {Status, Task, Msg}).

publisher_started(Module, Pid) ->
  gen_server:cast(?SERVER, {start, Module, Pid}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->

  proc_lib:init_ack({ok, self()}),

  Publishers = ?Config(status_publishers, []),
  lists:map(fun fyler_worker_sup:start_status_publisher/1, Publishers),
  gen_server:enter_loop(?MODULE, [], #state{}).

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({start, Module, Pid}, #state{publishers = Publishers} = State) ->
  {noreply, State#state{publishers = Publishers#{Module => Pid}}};

handle_cast({downloading, #task{id = Id}}, #state{publishers = Publishers} = State) ->
  Msg = status_msg(Id, downloading, #{}),
  send_to_publishers(Publishers, Msg),
  {noreply, State};

handle_cast({processing, #task{id = Id, size = Size, stats = Stats}}, #state{publishers = Publishers} = State) ->
  #{download_time := DTime} = Stats,
  Msg = status_msg(Id, processing, #{download_time => DTime, size => Size}),
  send_to_publishers(Publishers, Msg),
  {noreply, State};

handle_cast({uploading, #task{id = Id, stats = Stats}}, #state{publishers = Publishers} = State) ->
  #{process_time := PTime} = Stats,
  Msg = status_msg(Id, uploading, #{process_time => PTime}),
  send_to_publishers(Publishers, Msg),
  {noreply, State};

handle_cast({completed, #task{id = Id, stats = Stats, result = Result}}, #state{publishers = Publishers} = State) ->
  #{upload_time := UTime} = Stats,
  Msg = status_msg(Id, completed, #{upload_time => UTime, result => Result}),
  send_to_publishers(Publishers, Msg),
  {noreply, State};

handle_cast({error, #task{id = Id}, Error}, #state{publishers = Publishers} = State) ->
  Msg = status_msg(Id, error, #{errors => Error}),
  send_to_publishers(Publishers, Msg),
  {noreply, State};

handle_cast({aborted, #task{id = Id}}, #state{publishers = Publishers} = State) ->
  Msg = status_msg(Id, aborted, #{}),
  send_to_publishers(Publishers, Msg),
  {noreply, State};

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

status_msg(Id, Status, Data) ->
  Msg = #{id => Id, status => Status, data => Data#{worker_id => node()}},
  jsx:encode(Msg).

send_to_publishers(Publishers, Msg) ->
  SendFun = fun(Module, Pid) -> Module:publish_status(Pid, Msg) end,
  maps:map(SendFun, Publishers).
