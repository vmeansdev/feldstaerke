%%%-------------------------------------------------------------------
%%% @author vmeansdev
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Dec 2017 15:27
%%%-------------------------------------------------------------------
-module(log).
-author("vmeansdev").

-define(INFO,  "INFO").
-define(WARN,  "WARN").
-define(DEBUG, "DEBUG").
-define(ERROR, "ERROR").

%% API
-export([info/1,
         warn/1,
         debug/1,
         error/1]).

-export([info/2,
         warn/2,
         debug/2,
         error/2]).

info(Text) ->
    log(?INFO, Text).
info(ModuleFuncName, Text) ->
    log(?INFO, ModuleFuncName, Text).

warn(Text) ->
    log(?WARN, Text).
warn(ModuleFuncName, Text) ->
    log(?WARN, ModuleFuncName, Text).

debug(Text) ->
    log(?DEBUG, Text).
debug(ModuleFuncName, Text) ->
    log(?DEBUG, ModuleFuncName, Text).

error(Text) ->
    log(?ERROR, Text).
error(ModuleFuncName, Text) ->
    log(?ERROR, ModuleFuncName, Text).

log(Lvl, Text) ->
    io:format("[~p] ~p~n", [Lvl, Text]).
log(Lvl, ModuleFuncName, Text) ->
    io:format("[~p][~p]: ~p~n", [Lvl, ModuleFuncName, Text]).