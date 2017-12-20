%%%-------------------------------------------------------------------
%%% @author vmeansdev
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Dec 2017 16:35
%%%-------------------------------------------------------------------
-module(feldstaerke_parser).
-author("vmeansdev").

-include("../headers/feldstaerke.hrl").

%% API
-export([parse_msg/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec parse_msg(binary()) -> ok.
parse_msg(Msg) ->
    log:info(?MFN, "Message received"),
    log:debug(?MFN, Msg),
    UpdateBody  = jsx:decode(Msg),
    log:info(?MFN, "Message parsed with jsx..."),
    log:debug(?MFN, UpdateBody),

    [_ | Inf] = UpdateBody,
    [{MsgType, _MsgBody} | _] = Inf,

    log:debug(?MFN, MsgType),
    case MsgType of
        <<"message">> ->
            io:format("MsgType matched to <<message>>~n"),
            handle_message_body(Inf);
        <<"callback_query">> ->
            io:format("MsgType matched to <<callback_query>>~n"),
            handle_callback_query(Inf);
        _ ->
            io:format("MsgType matched to *~n"),
            validate_message(true, 0)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_message_body(Body) ->
    Message     = get_value(<<"message">>, Body),
    UserId      = get_value(<<"id">>, get_value(<<"from">>, Message)),
    Username    = get_value(<<"username">>, get_value(<<"from">>, Message)),
    ChatId      = get_value(<<"id">>, get_value(<<"chat">>, Message)),
    MessageText = get_value(<<"text">>, Message),

    Reply       = {UserId, Username, ChatId, MessageText},

    RList       = tuple_to_list(Reply),
    ParseFailed = lists:member(undefined, RList),

    validate_message(ParseFailed, Reply).

handle_callback_query(Body) ->
    Query     = get_value(<<"callback_query">>, Body),
    UserId    = get_value(<<"id">>, get_value(<<"from">>, Query)),
    Username  = get_value(<<"username">>, get_value(<<"from">>, Query)),
    Message   = get_value(<<"message">>, Query),
    ChatId    = get_value(<<"id">>, get_value(<<"chat">>, Message)),
    Data      = get_value(<<"data">>, Query),

    Command0  = "EnterShopID = " ++ binary_to_list(Data),
    Command   = list_to_binary(Command0),

    Reply     = {UserId, Username, ChatId, Command},

    RList       = tuple_to_list(Reply),
    ParseFailed = lists:member(undefined, RList),

    validate_message(ParseFailed, Reply).


-spec get_value (term(), undefined) -> undefined;
    (binary(), [term()]) -> term().
get_value(_, undefined) -> undefined;
get_value(Key, Data) ->
    proplists:get_value(Key, Data).

-spec validate_message(false, tuple()) -> ok;
                             (true, _) -> ok.
validate_message(false, {UserId, Username, ChatId, MessageText}) ->
    UserName = binary:bin_to_list(Username),
    MsgText  = binary:bin_to_list(MessageText),
    feldstaerke_processor:process_msg({UserId, UserName, ChatId, MsgText});

validate_message(true, _) ->
    log:error(?MFN, "Parser error!"),
    ok.