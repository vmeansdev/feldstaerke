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
    {ok, { {one_for_one, 20, 10}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
