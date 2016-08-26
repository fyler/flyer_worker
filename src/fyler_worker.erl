%% Copyright
-module(fyler_worker).

-export([start/0, stop/0, upgrade/0, ping/0]).

-define(APPS, [lager, erlexec]).

start() ->
	ulitos_app:ensure_started(?APPS),
	application:start(fyler_worker).

stop() ->
	application:stop(fyler_worker).

upgrade() ->
	ulitos_app:reload(fyler_worker),
	ok.
 
ping() ->
	pong.



