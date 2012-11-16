-module(packet_handler).
-author("zebbey@gmail.com").
-vsn("1.0.0").

-export([handle_packet/2]).

handle_packet(_State, Packet) ->
    io:format("~p~n", [Packet]),
    ok.
