%%%-------------------------------------------------------------------
%% @doc feldstaerke public API
%% @end
%%%-------------------------------------------------------------------

-module(feldstaerke_app).

-behaviour(application).
-include("./headers/feldstaerke.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

-spec start(_, _) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    log:info(?MFN, "app started"),
    feldstaerke_sup:start_link().

%%--------------------------------------------------------------------
-spec stop(_) -> ok.
stop(_State) ->
    log:info(?MFN, "app stopped"),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
