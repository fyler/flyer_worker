-module(fyler_worker_amqp).
-behaviour(gen_server).

-include("../include/log.hrl").
-include("../include/fyler.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-define(SERVER, fyler_task_source).

-record(state, {
    host  = "" :: string(),
    user = <<"">> :: binary(),
    pass = <<"">> ::binary(),
    category :: atom(),
	connection :: undefined | pid(), 
	channel :: undefined | pid()
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Opts) ->
    Host = maps:get(amqp_host, Opts, "localhost"),
    User = maps:get(amqp_user, Opts, <<"guest">>),
    Pass = maps:get(amqp_pass, Opts, <<"guest">>),
    Category = maps:get(category, Opts, undefined),
    self() ! start,
    {ok, #state{host = Host, user = User, pass = Pass, category = Category}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, #state{host = Host, user = User, pass = Pass, category = Category_} = State) ->
    Category = atom_to_binary(Category_, utf8),
    {ok, Connection} = amqp_connection:start(#amqp_params_network{host = Host, username = User, password = Pass}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    amqp_channel:call(Channel, #'queue.declare'{queue = Category, durable = true}),
    amqp_channel:call(Channel, #'basic.qos'{prefetch_count = 1}),
    #'basic.consume_ok'{} = amqp_channel:subscribe(Channel, #'basic.consume'{queue = Category}, self()),
    {noreply, State#state{connection = Connection, channel = Channel}};

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

handle_info(#'basic.cancel_ok'{}, State) ->
    {noreply, State};

handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Payload}}, State) ->
	Message = binary_to_term(Payload),
    case Message of
        #task{meta = Meta} = Task ->
            fyler_worker_server:run_task(Task#task{meta = Meta#{tag => Tag}});
        _ ->
            ok
    end,
    {noreply, State};

handle_info(#task{meta = #{tag := Tag}}, #state{channel = Channel} = State) ->
	amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
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

