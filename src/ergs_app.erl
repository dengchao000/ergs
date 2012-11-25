-module(ergs_app).
-author("zebbey@gmail.com").
-behaviour(application).

-export([start/0,
	 start/2,
	 stop/1]).

start() ->
    application:start(ergs).

start(_StartType, StartArgs) ->
    ergs_sup:start_link(StartArgs).

stop(_State) ->
    ok.

