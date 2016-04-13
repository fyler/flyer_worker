-module(fyler_task_amqp).
-behaviour(gen_event).

-include_lib("fyler_worker/include/log.hrl").
-include_lib("fyler_worker/include/fyler.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {
	connection :: undefined | pid(),
	channel :: undefined | pid(),
  tag
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, listen/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
  terminate/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  Pid = spawn_link(?MODULE, listen, [self()]),
  {ok, Pid}.

listen(Handler) ->
  fyler_worker_event:add_handler(?MODULE, Handler),
  receive
    _Msg -> ok
  end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->

    proc_lib:init_ack({ok, self()}),

    Opts = ?Config(amqp, #{}),
    Host = maps:get(amqp_host, Opts, "localhost"),
    User = maps:get(amqp_user, Opts, <<"guest">>),
    Pass = maps:get(amqp_pass, Opts, <<"guest">>),
    Category = atom_to_binary(?Config(category, undefined), utf8),

    {ok, Connection} = amqp_connection:start(#amqp_params_network{host = Host, username = User, password = Pass}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    amqp_channel:call(Channel, #'queue.declare'{queue = Category, durable = true}),
    amqp_channel:call(Channel, #'basic.qos'{prefetch_count = 1}),
    #'basic.consume_ok'{} = amqp_channel:subscribe(Channel, #'basic.consume'{queue = Category}, self()),

    gen_event:enter_loop(?MODULE, [], #state{connection = Connection, channel = Channel}).

handle_event(#fevent{type = completed}, #state{channel = Channel, tag = Tag} = State) ->
  amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
  {ok, State};

handle_event(#fevent{type = error}, #state{channel = Channel, tag = Tag} = State) ->
  amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
  {ok, State};

handle_event(_Event, State) ->
  {ok, State}.

handle_call(_, State) ->
  {ok, ok, State}.

handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Payload}}, State) ->
  Task = fyler_task_utils:decode_task(Payload),
    case Task of
        #task{}->
          fyler_task_manager:task(Task),
          {noreply, #state{tag = Tag} = State};
        _ ->
          {noreply, State}
    end;

handle_info(#'basic.consume_ok'{}, State) ->
  {noreply, State};

handle_info(#'basic.cancel_ok'{}, State) ->
  {noreply, State};

handle_info(_, State) ->
  {ok, State}.

terminate(_Reason, #state{connection = Connection, channel = Channel}) ->
  amqp_channel:close(Channel),
  amqp_connection:close(Connection),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

