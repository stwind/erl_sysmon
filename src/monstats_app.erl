-module(monstats_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = monstats_sup:start_link(),
    monstats_riak_sysmon_handler:add_handler(),
    {ok, Sup}.

stop(_State) ->
    ok.
