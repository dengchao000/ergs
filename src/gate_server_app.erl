-module(gate_server_app).
-author("zebbey@gmail.com").
-behaviour(application).

-export([start/2, stop/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, once}, {reuseaddr, true}]).

start(_StartType, _StartArgs) ->
    ListenPort = get_app_env(listen_port, 7878),
    {ok, LSocket} = gen_tcp:listen(ListenPort, ?TCP_OPTIONS),
    case gate_server_sup:start_link(LSocket) of
	{ok, Pid} ->
	    gate_server_sup:start_child(),
	    {ok, Pid};
	Other ->
	    {error, Other}
    end.

stop(_State) ->
    ok.

get_app_env(Option, Default) ->
    case application:get_env(application:get_application(), Option) of
	{ok, Value} ->
	    Value;
	_ ->
	    case init:get_argument(Option) of
		[[Value|_]] ->
		    Value;
		error ->
		    Default
	    end
    end.
