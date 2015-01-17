-module(monstats_riak_sysmon_handler).

-behaviour(gen_event).

%% API
-export([add_handler/0, get_call_count/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, {
          count = 0 :: integer()
         }).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

add_handler() ->
    riak_sysmon_filter:add_custom_handler(?MODULE, []).

get_call_count() ->
    riak_sysmon_filter:call_custom_handler(?MODULE, get_call_count, infinity).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init([]) ->
    init_metrics(),
    {ok, #state{}}.

handle_event({monitor, _, ProcType, Value}, State) when ProcType == long_gc; 
                                                        ProcType == large_heap; 
                                                        ProcType == long_schedule ->
    exometer:update([sysmon, node(), ProcType, times], 1),
    exometer:update([sysmon, node(), ProcType, value], Value),
    {ok, incr_count(State)};

handle_event({monitor, _, PortType, Port}, State) when PortType == busy_port; 
                                                       PortType == busy_dist_port ->
    log_busy_port(Port),
    exometer:update([sysmon, node(), PortType, times], 1),
    {ok, incr_count(State)};

handle_event(Event, State) ->
    error_logger:info_msg("Unhandled event ~p", [Event]),
    {ok, State}.

handle_call(get_call_count, State) ->
    Reply = State#state.count,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_metrics() ->
    exometer:new([sysmon, node(), long_gc, times], counter),
    exometer:new([sysmon, node(), long_gc, value], gauge),
    exometer:new([sysmon, node(), large_heap, times], counter),
    exometer:new([sysmon, node(), large_heap, value], gauge),
    exometer:new([sysmon, node(), long_schedule, times], counter),
    exometer:new([sysmon, node(), long_schedule, value], gauge),
    exometer:new([sysmon, node(), busy_port, times], counter),
    exometer:new([sysmon, node(), busy_dist_port, times], counter),
    ok.

incr_count(#state{count = Count} = State) ->
    State#state{count = Count + 1}.

log_busy_port(Port) ->
    error_logger:error_msg("Busy port detected ~p",[Port]).
