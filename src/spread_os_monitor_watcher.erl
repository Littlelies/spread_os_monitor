%%%-------------------------------------------------------------------
%% @doc spread_os_monitor_watcher
%% @end
%%%-------------------------------------------------------------------
-module(spread_os_monitor_watcher).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    self
}).

-define(PEERS_ROOT_PATH, <<"os_monitor">>).
-define(SELF, atom_to_binary(node(), utf8)).

-define(INTERVAL, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

start_link() ->
    %% Make sure the cron job is started
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:send_after(?INTERVAL, self(), trigger),
    {ok, #state{self = ?SELF}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(trigger, State) ->
    erlang:send_after(?INTERVAL, self(), trigger),

    %% Check memory
    [_, Total, Free, _] = re:split(os:cmd("vmstat -s | grep 'total memory\\|free memory'"), "[^\\d]+", [{return, list}]),

    %% Check disk
    RawDiskStat = re:split(os:cmd("df --output=target,pcent | tail -n +2"), "[ \n]+", [{return, list}]),
    DiskStat = parse_diskstat(RawDiskStat, []),

    %% Check idle total CPU
    Idle = os:cmd("mpstat 1 1 | grep Average | awk -F ' ' '{print $12}'"),

    Report = [
        {<<"memory">>,
            [{<<"total">>, Total}, {<<"free">>, Free}]
        },
        {<<"disk">>, DiskStat},
        {<<"idle_cpu">>, Idle}
    ],
    lager:info("Encoding ~p", [Report]),
    JsonReport = jsx:encode(Report),

    spread:post([<<"os_monitor">>, State#state.self], JsonReport),
    {noreply, State};
handle_info(_Info, State) ->
    lager:info("UNKNOWN Update ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
parse_diskstat([_], Acc) ->
    Acc;
parse_diskstat([Disk, Use | DiskStats], Acc) ->
    parse_diskstat(DiskStats, [{Disk, Use} | Acc]).
