-module(fyler_status_amqp).
-behaviour(gen_server).

-include_lib("fyler_worker/include/log.hrl").
-include_lib("fyler_worker/include/fyler.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-define(SERVER, fyler_status_queue).
-define(QUEUE, <<"status">>).

-record(state, {
  host  = "" :: string(),
  user = <<"">> :: binary(),
  pass = <<"">> ::binary(),
  connection :: undefined | pid(), 
  channel :: undefined | pid()
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, publish_status/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link(?MODULE, [], []).

publish_status(Pid, Status) ->
  gen_server:cast(Pid, {publish, Status}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  
  proc_lib:init_ack({ok, self()}),

  Opts = ?Config(amqp, #{}),
  Host = maps:get(amqp_host, Opts, "localhost"),
  User = maps:get(amqp_user, Opts, <<"guest">>),
  Pass = maps:get(amqp_pass, Opts, <<"guest">>),

  {ok, Connection} = amqp_connection:start(#amqp_params_network{host = Host, username = User, password = Pass}),
  {ok, Channel} = amqp_connection:open_channel(Connection),
  amqp_channel:call(Channel, #'queue.declare'{queue = ?QUEUE, durable = true}),

  fyler_status_publisher:publisher_started(?MODULE, self()),

  gen_server:enter_loop(?MODULE, [], #state{connection = Connection, channel = Channel}).

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({publish, Status}, #state{channel = Channel} = State) ->
  Msg = #amqp_msg{payload = Status},
  amqp_channel:cast(Channel, #'basic.publish'{exchange = <<"">>, routing_key = ?QUEUE}, Msg),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(#'basic.cancel_ok'{}, State) ->
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{connection = Connection, channel = Channel}) ->
  amqp_channel:close(Channel),
  amqp_connection:close(Connection),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------