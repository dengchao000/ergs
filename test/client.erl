-module(client).
-export([start/0, test/0]).
-include_lib("kernel/include/file.hrl").

-define(CLIENT_NUM, 500).

test() ->
    spawn(fun() -> test_proc(0) end).
		  
test_proc(Count) ->
    NewCount = Count + 1,
    if NewCount >= ?CLIENT_NUM ->
	    io:format("total: ~p~n", [NewCount]);
       true ->
	    spawn(fun() -> test_loop() end),
	    test_proc(NewCount)
    end.

test_loop() ->
    timer:kill_after(5000, self()).

start() ->
    CounterPid = spawn(fun() -> counter(0, 0) end),
    spawn(fun() -> connect(CounterPid, 0, os:timestamp()) end),
    CounterPid.

connect(CounterPid, Count, TimeStamp) ->
    case gen_tcp:connect("localhost", 7878, [binary, {packet, 0}]) of
	{ok, Socket} ->
	    NewCount = Count + 1,
	    if 
		NewCount < ?CLIENT_NUM + 1 ->
		    %io:format("connect ok.(count: ~p)~n", [NewCount]),
		    spawn(fun() -> connect(CounterPid, NewCount, os:timestamp()) end),
		    loop(CounterPid, Socket, 0, TimeStamp);
	        true ->
		    io:format("connect count full~n")
	    end;
	{error, Info} ->
	    io:format("error: connect failed.[info: ~p]~n", [Info])
    end.

loop(CounterPid, Socket, Count, TimeStamp) ->
    ok = gen_tcp:send(Socket, term_to_binary("Hello")),
    receive
	{tcp, Socket, Packet} ->
	    NewCount = Count + 1,
	    Packet,
	    %io:format("Client received binary = ~p~n", [Packet]),
	    %Val = binary_to_term(Packet),
	    %io:format("Client result = ~p~n",[Val]),
	    TimeDiff = timer:now_diff(os:timestamp(), TimeStamp),
	    %timer:sleep(1000),
	    if
		TimeDiff < 5000000 ->
		    loop(CounterPid, Socket, NewCount, TimeStamp);
		true ->
		    %io:format("Count: ~p~n", [NewCount])
		    CounterPid ! NewCount
		     
	    end
    end.

counter(Count, ClientCount) ->
    receive
	AddCount ->
	    NowCount = Count + AddCount,
	    NowClientCount = ClientCount + 1,
	    if
		NowClientCount >= ?CLIENT_NUM ->
		    io:format("total: ~p~n", [NowCount]),
		    timer:kill_after(0, self());
		true ->
		    counter(NowCount, NowClientCount)
	    end
    end.


