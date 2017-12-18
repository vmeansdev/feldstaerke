%%%-------------------------------------------------------------------
%% @doc feldstaerke top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(feldstaerke_sup).

-behaviour(supervisor).

-include("./headers/feldstaerke.hrl").

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
    log:info(?MFN, "Supervisor init called"),
    {ok, { {one_for_one, 20, 10}, [
        {feldstaerke_parser, {feldstaerke_parser, start_link, []},
         permanent, 2000, worker, [feldstaerke_parser]},

        {feldstaerke_processor, {feldstaerke_processor, start_link, []},
         permanent, 2000, worker, [feldstaerke_processor]},

        {feldstaerke_shopsm, {feldstaerke_shopsm, start_link, []},
         permanent, 2000, worker, [feldstaerke_shopsm]}
    ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
