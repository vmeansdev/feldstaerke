%%%-------------------------------------------------------------------
%%% @author vmeansdev
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Dec 2017 22:44
%%%-------------------------------------------------------------------
-module(bot_cowboy_handler).
-author("vmeansdev").

-include("../headers/feldstaerke.hrl").

%% API
-export([init/2]).

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req0, State) ->
    {ok, Data, _} = cowboy_req:read_body(Req0),
    Req = cowboy_req:reply(200,
        #{}, %% Headers
        "" , %% Body
        Req0),
    log:info(?MFN, "Init called, now call parser parse_msg..."),
    feldstaerke_parser:parse_msg(Data),

    {ok, Req, State}.
