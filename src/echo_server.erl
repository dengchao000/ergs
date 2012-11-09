-module(echo_server).

-export([start/0, loop/1]).

start() ->
    tcp_server:start_link(?MODULE, 1234, {?MODULE, loop}).

loop(Socket) ->
    gen_tcp:setopts(Socket, {active, once}),
    case gen_tcp:recv(Socket, 0) of
	{ok, Msg} ->
	    gen_tcp:send(Socket, Msg),
	    loop(Socket);
	{error, Reason} ->
	    io:format('error: ~p~n', [Reason])
    end.


