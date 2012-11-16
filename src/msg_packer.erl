-module(msg_packer).
-author("zebbey@gmail.com").
-vsn("1.0.0").

-include("tcp_server.hrl").

-export([proc_msg/2]).

proc_msg(#conn_state{msg = RemainMsg}, Msg) ->
    NewPack = <<RemainMsg/binary, Msg/binary>>,
    case msgpack:unpack(NewPack) of
	{error, incomplete} ->
	    more;
	{Packet, Remain} ->
	    {Packet, Remain}
    end.
