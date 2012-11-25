-module(ergs_sup).
-author("zebbey@gmail.com").
-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
    RestartStrategy = one_for_one,
    AllowedRestarts = 10,
    MaxSeconds = 30,

    SupFlags = {RestartStrategy, AllowedRestarts, MaxSeconds},
    Restart = permanent,
    Shutdown = 2000,
    
    Listener = {ergs_listener, {ergs_listener, start_link, [Args]}, Restart, Shutdown, worker, dynamic},
    %ConnSup = {ergs_conn_sup, {ergs_conn_sup, start_link, []}, Restart, infinity, supervisor, dynamic},

    {ok, {SupFlags, [Listener]}}.




