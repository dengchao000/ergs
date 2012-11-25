-module(ergs_listener).
-author("zeb <zebbey@gmail.com>").
-behaviour(gen_server).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, once}, {reuseaddr, true}]).
-record(state, {lsocket, module}).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-export([start_link/1]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(_Args) ->
    Port = ergs_util:get_app_env(port, 7878),
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
	{ok, LSocket} ->
	    spawn_listeners(LSocket),
	    {ok, #state{lsocket = LSocket}};
	Error ->
	    {stop, Error}
    end.

handle_call({connected, ClientSocket}, _From, State) ->
    Result = on_connected(ClientSocket),
    {reply, Result, State};
	
handle_call(Msg, _From, State) ->
    error_logger:error_report([{undefined_call, Msg}]),
    {reply, ok, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    io:format("listener terminate ~p ~p.~n", [Reason, State]),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

spawn_listeners(LSocket) ->
    ListenNum = ergs_util:get_app_env(listen_num, 5),
    spawn_listeners(LSocket, ListenNum).

spawn_listeners(_LSocket, 0) ->
    done;

spawn_listeners(LSocket, Num) ->
    spawn_link(?MODULE, proc_listen, [LSocket]),
    spawn_listeners(LSocket, Num - 1).

proc_listen(LSocket) ->
    case gen_tcp:accept(LSocket) of
	{ok, ClientSocket} ->
	    case gen_server:call(?MODULE, {connected, ClientSocket}) of
		{ok, Pid} ->
		    gen_tcp:controlling_process(ClientSocket, Pid),
		    proc_listen(LSocket);
		Error ->
		    error_logger:info_report([{proc_listen, Error}]),
		    proc_listen(LSocket)
	    end;
	Error ->
	    error_logger:info_report([{proc_listen, Error}]),
	    proc_listen(LSocket)
    end.

on_connected(ClientSocket) ->
    self().

