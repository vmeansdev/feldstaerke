%%%-------------------------------------------------------------------
%%% @author vmeansdev
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Dec 2017 19:50
%%%-------------------------------------------------------------------
-module(feldstaerke_shopsm).
-author("vmeansdev").

-behaviour(gen_fsm).

-include("../headers/feldstaerke.hrl").

%% API
-export([start_link/0, stop/0]).
-export([show_shop_list/1, request_user_location/1]).

%% gen_fsm callbacks
-export([init/1,
    wait_for_action/2,
    shop_choosing/2,
    location_requesting/2,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {shops = [], chat_id}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_fsm:send_all_state_event(?SERVER, stop).

show_shop_list(ChatID) ->
    gen_fsm:send_event(?SERVER, {shop_choosing, ChatID}).

request_user_location(ShopID) ->
    gen_fsm:send_event(?SERVER, {chosen, ShopID}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init([]) ->
    {ok, wait_for_action, {}}.

wait_for_action(Event, _State0) ->
    case Event of
        {shop_choosing, ChatID} ->
            State = #state{shops = default_shop_list(), chat_id = ChatID},
            choose_shop(State),
            {next_state, shop_choosing, State};
        _ ->
            log:debug(?MFN, "hanlded wildcard event")
    end.

shop_choosing(Event, State) ->
    case Event of
        {chosen, ShopID} ->
            io:format("Great choice! ~p~n", [ShopID]),
            request_location(State, ShopID),
            {next_state, location_requesting, State};
        _ ->
            io:format("Already choosing shop"),
            choose_shop(State),
            {next_state, shop_choosing, State}
    end.

location_requesting(Event, State) ->
    case Event of
        _ ->

            {next_state, items_reporting, State}
    end.

handle_event(stop, _StateName, State) ->
    {stop, normal, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
default_shop_list() ->
    [#{address  => "71 Pilgrim Avenue Chevy Chase, MD 20815",
        id      => 1,
        visited => false,
        latlng  => {1,1}},
     #{address  => "44 Shirley Ave. West Chicago, IL 60185",
       id       => 2,
       visited  => false,
       latlng  => {2,2}},
     #{address  => "514 S. Magnolia St. Orlando, FL 32806",
       id       => 3,
       visited  => false,
       latlng  => {3,3}}
    ].

choose_shop(State) ->
    ChatID = State#state.chat_id,
    ShopList = [Shop || Shop <- State#state.shops, maps:get(visited, Shop) =/= true],
    log:debug(?MFN, "Now call send_buttons"),
    Message = case length(ShopList) of
                  0 ->
                      "No shops available. Good work :)";
                  _ ->
                      "Shops available:\n\nPlease choose one when you are inside"
              end,
    feldstaerke_processor:send_buttons(ChatID, Message, ShopList).

request_location(State, ShopID) ->
    ChatID = State#state.chat_id,
    [Shop] = lists:filter(fun(S) -> Id = maps:get(id, S), Id =:= ShopID end, State#state.shops),

    Address = maps:get(address, Shop),

    Msg = "Your choice is\n\n" ++ Address,
    feldstaerke_processor:request_location(ChatID, Msg, ShopID).