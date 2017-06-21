%%%-------------------------------------------------------------------
%% @doc spread_os_monitor public API
%% @end
%%%-------------------------------------------------------------------

-module(spread_os_monitor_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    spread_os_monitor_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
