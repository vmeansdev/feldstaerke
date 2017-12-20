%%%-------------------------------------------------------------------
%%% @author vmeansdev
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Dec 2017 18:26
%%%-------------------------------------------------------------------
-module(feldstaerke_processor).
-author("vmeansdev").

-include("../headers/feldstaerke.hrl").

%% API
-export([process_msg/1,
         send_buttons/3,
         request_location/3]).

-type message() :: {integer(), [byte()], integer(), [byte()]}.

%%%===================================================================
%%% API
%%%===================================================================
-spec process_msg(message()) -> {reply, ok, []}.
process_msg(Request) ->
    log:debug(?MFN, Request),
    {UserId, UserName, ChatId, MessageText} = Request,
    UserState = check_user_state(UserId),

    case UserState of
        ?UNAUTHORIZED ->
            log:info(?MFN, "Received message in unauthorized state"),
            log:info(?MFN, MessageText),
            NormMsg = normalize_command(MessageText),
            reply_to_unauthorized(UserId, UserName, ChatId, NormMsg);
        ?AUTHORIZED ->
            NormMsg = normalize_command(MessageText),
            handle_command(UserId, UserName, ChatId, NormMsg)
    end.

send_buttons(ChatID, Message, Shops) ->
    Buttons = inline_buttons(Shops),
    send_reply(ChatID, Message, Buttons).

request_location(ChatID, Message, _ShopID) ->
    send_reply(ChatID, Message, location_req()).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc return current user state from ETS
-spec check_user_state(integer()) -> string().
check_user_state(UserId) ->
    UserState = case ets:member(usertable, UserId) of

                    false ->
                        ets:insert(usertable, {UserId, ?UNAUTHORIZED}),
                        ?UNAUTHORIZED;

                    true ->
                        [{_, UserState1}] = ets:lookup(usertable, UserId),
                        UserState1

                end,
    log:debug(?MFN, UserState),
    UserState.

%% @doc Sends message to certain chat id
send_reply(ChatId, Reply, Options) ->
    ErlBody = #{chat_id => ChatId,
                text    => list_to_binary(Reply)},
    log:info(?MFN, "Options"),
    MergedBody = maps:merge(ErlBody, Options),
    log:debug(?MFN, MergedBody),
    send_reply(MergedBody).

-spec send_reply(integer(), string()) -> ok.
send_reply(ChatId, Reply) ->
    ErlBody = #{chat_id => ChatId,
                text    => list_to_binary(Reply)},
    send_reply(ErlBody).

send_reply(ErlBody) ->
    {ok, Path} = application:get_env(?APPLICATION, token),

    Url = "https://api.telegram.org/bot"++Path++"/sendMessage",
    Headers = [],
    ContentType = "application/json",
    Body = binary_to_list(jsx:encode(ErlBody)),

    log:debug(?MFN,Body),

    Request = {Url, Headers, ContentType, Body},
    log:debug(?MFN, Request),
    httpc:request(post, Request, [], []),
    ok.

%% @doc Authorize user - change state to "authorized"
-spec do_authorization(integer()) -> true.
do_authorization(UserId) ->
    ets:insert(usertable, {UserId, ?AUTHORIZED}).

%% @doc Cut @<bot_name> from message
-spec normalize_command([byte()]) -> [byte()].
normalize_command(Command) ->
    log:info(?MFN, Command),
    lists:nth(1, re:split(Command, "[@]", [{return, list}])).

%% @doc Reply for unauthorized user
reply_to_unauthorized(UserId, _Username, ChatId, "/start") ->
    do_authorization(UserId),
    log:debug(?MFN, "Now call shopsm start_link.."),
    {ok, Pid} = start_shop_fsm_for(UserId),
    feldstaerke_shopsm:show_shop_list(Pid, ChatId),
    ok;

reply_to_unauthorized(_UserId, _Username, ChatId, _) ->
    send_reply(ChatId, "Hello, fellow!\n\nIt is a proof of concept of fieldforce bot written in Erlang!\n\nBegin with /start").

start_shop_fsm_for(UserId) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    Spec = {feldstaerke_shopsm, {feldstaerke_shopsm, start_link, [UserId]},
        Restart, Shutdown, Type, [feldstaerke_shopsm]},
    supervisor:start_child(feldstaerke_sup, Spec).

inline_buttons(Buttons) ->
    ErlTerm = #{reply_markup =>
    #{inline_keyboard =>
            [[#{text => list_to_binary(Text), callback_data => integer_to_binary(CD)}]
             || #{address := Text, id := CD} <- Buttons
            ]
        }
    },
    ErlTerm.

location_req() ->
    #{reply_markup =>
        #{one_time_keyboard => true,
          keyboard => [[#{
              text => <<"Send us your location to prove you are there">>,
              request_location => true
          }]]
        }
    }.

%% ------------------------------------------------------------------
%% Command Handlers
%% ------------------------------------------------------------------
-spec handle_command(integer(), string(), integer(), string()) -> any().
handle_command(_UserId, _Username, ChatId, "/start") ->
    send_reply(ChatId, "Session already started.");

handle_command(_UserId, _Username, ChatId, "/help") ->
    send_reply(ChatId, "Commands available:\n\n/start - to start work with bot\n/shops - to see shops list to visit\n/exit to finish work");

handle_command(UserId, _Username, ChatId, "/shops") ->
    log:debug(?MFN, UserId),
    Pid = gproc:lookup_local_name(UserId),
    log:debug(?MFN, Pid),
    feldstaerke_shopsm:show_shop_list(Pid, ChatId),
    ok;

handle_command(UserId, Username, ChatId, "/exit") ->
    ets:insert(usertable, {UserId, "unauthorized"}),
    Pid = gproc:lookup_local_name(UserId),
    feldstaerke_shopsm:stop(Pid),
    gproc:unregister_name(UserId),
    send_reply(ChatId, Username ++ ", logout complete.");

handle_command(UserId, _Username, _ChatId, "EnterShopID = " ++ ShopID0) ->
    ShopID = list_to_integer(ShopID0),
    Pid = gproc:lookup_local_name(UserId),
    feldstaerke_shopsm:request_user_location(Pid, ShopID),
    ok;

handle_command(_UserId, _Username, ChatId, _) ->
    send_reply(ChatId, "Oops! Don't know such command.\n\nUse /help to see what I can").