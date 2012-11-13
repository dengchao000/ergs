
-module(gate_server_sup).
-author("zebbey@gmail.com").
-behaviour(supervisor).

-export([start_link/1, start_child/0]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link(LSocket) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [LSocket]).

start_child() ->
    supervisor:start_child(?MODULE, []).
    
init([LSocket]) ->
    Server = {tcp_server, {tcp_server, start_link, [LSocket]}, temporary, brutal_kill, worker, [tcp_server]},
    Children = [Server],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children} }.

