-module(fyler_worker_app).
-behaviour(application).

-include("../include/fyler.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	lager:info("Starting application: fyler_worker"),
    ConfigPath = case ?Config(config, undefined) of
      undefined -> "fyler_worker.config";
      Else -> Else
    end,
    ulitos_app:load_config(fyler_worker, ConfigPath, ["/etc"]),
    fyler_worker_sup:start_link().

stop(_State) ->
    ok.
