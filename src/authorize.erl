-module(authorize).
-author("zebbey@gmail.com").
-vsn("1.0.0").

-include("tcp_server.hrl").

-export([authorizing/2]).

authorizing(State, Msg) ->
    case proc_msg(State, Msg) of
	{more, NewMsg} ->
	    io:format("need more ~n"),
	    {more, NewMsg};
	{Packet, Remain} ->
	    io:format("packet: ~p~n", [Packet]),
	    case do_auth(Packet) of
		ok ->
		    {ok, Remain};
		invalid ->
		    io:format("invalid~n"),
		    invalid
	    end
    end.

do_auth(Packet) ->
    case Packet of
	[[<<"id">>, Id], [<<"passwd">>, _Passwd], [<<"sign">>, _Sign]] ->
	    io:format("match ok~n"),
	    if Id == <<"zeb">> ->
		    io:format("ok: ~p~n", [Packet]),
		    ok;
	       true ->
		    invalid
	    end;
	_ ->
	    invalid
    end.

proc_msg(#conn_state{msg = RemainMsg}, Msg) ->
    NewMsg = <<RemainMsg/binary, Msg/binary>>,
    case msgpack:unpack(NewMsg) of
	{error, incomplete} ->
	    {more, NewMsg};
	{Packet, Remain} ->
	    {Packet, Remain}
    end.
