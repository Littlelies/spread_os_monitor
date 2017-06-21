%%%-------------------------------------------------------------------
%% @doc spread_os_monitor top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(spread_os_monitor_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Watcher = {spread_os_monitor_worker, {spread_os_monitor_worker, start_link, []}, permanent, 5000, worker, [spread_os_monitor_worker]},
    {ok, { {one_for_one, 5, 1}, [Watcher]} }.

%%====================================================================
%% Internal functions
%%====================================================================
