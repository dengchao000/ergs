
-module(tcp_server).
-author('zeb <zebbey@gmail.com>').
-vsn('1.0.0').

-include("tcp_server.hrl"). 
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([accept_loop/1]).

-export([start_link/3]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, once}, {reuseaddr, true}]).

-record(conn_state, {port, loop, ip = any, lsocket = null}).

%%---------------------------
%% interface
%%---------------------------
start_link(Name, Port, Loop) ->
    State = #conn_state{port = Port, loop = Loop},
    gen_server:start_link({local, Name}, ?MODULE, State, []).

%%---------------------------
%% callbacks
%%---------------------------
init(State = #conn_state{port = Port}) ->
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
	{ok, LSocket} ->
	    NewState = State#conn_state{lsocket = LSocket},
	    {ok, accept(NewState)};
	{error, Reason} ->
	    {stop, Reason}
    end.

handle_cast({accepted, _Pid}, State = #conn_state{}) ->
    {noreply, accept(State)}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVer, State, _Extra) ->
    {ok, State}.

%%---------------------------
%% private
%%---------------------------
accept_loop({Pid, LSocket, {Mod, Loop}}) ->
    case gen_tcp:accept(LSocket) of
	{ok, Socket} ->
	    gen_server:cast(Pid, {accepted, self()}),
	    Mod:Loop(Socket);
	{error, Reason} ->
	    io:format('accept error: ~p~n', [Reason])
    end.

accept(State = #conn_state{lsocket = LSocket, loop = Loop}) ->
    spawn(?MODULE, accept_loop, [{self(), LSocket, Loop}]),
    State.
