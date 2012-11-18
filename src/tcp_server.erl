-module(tcp_server).
-author("zeb <zebbey@gmail.com>").
-vsn("1.0.0").
-behaviour(gen_server).

-include("tcp_server.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/1]).

start_link(LSocket) ->
    State = #conn_state{lsocket = LSocket},
    gen_server:start_link(?MODULE, State, []).

init(#conn_state{lsocket = LSocket}) ->
    {ok, #conn_state{lsocket = LSocket}, 0}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, Msg}, #conn_state{state = Auth, msg = RemainMsg} = State) ->
    inet:setopts(Socket, [{active, once}]),

    case Auth of
	authorized ->
	    NewMsg = <<RemainMsg/binary, Msg/binary>>,
	    io:format("authorized: ~p~n", [NewMsg]),
	    {noreply, State};
	unauthorized ->
	    case authorize:authorizing(State, Msg) of
		{ok, Remain} ->
		    {noreply, State#conn_state{state = authorized, msg = Remain}};
		{more, Remain} ->
		    {noreply, State#conn_state{msg = Remain}};
		_ ->
		    {stop, normal, State}
	    end
%%	    case msg_packer:proc_msg(State, Msg) of
%%		more ->
%%		    NewMsg = <<RemainMsg/binary, Msg/binary>>,
%%		    NewState = State#conn_state{msg = NewMsg},
%%		    {noreply, NewState};
%%		
%%		{Packet, Remain} ->
%%		    case packet_handler:handle_packet(State, Packet, Remain) of
%%			{ok, NewState} ->
%%			    {noreply, NewState};
%%			unauthorized ->
%%			    {stop, normal, State}
%%		    end
%%	    end
    end;

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info(timeout, #conn_state{lsocket = LSocket} = State) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    {ok, {IP, _Port}} = inet:peername(Socket),
    gate_server_sup:start_child(),
    {noreply, State#conn_state{state = unauthorized, socket = Socket, addr = IP, msg = <<>>}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #conn_state{socket = Socket}) ->
    (catch gen_tcp:close(Socket)),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

    

