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

-behaviour(gen_server).

-include("../headers/feldstaerke.hrl").

%% API
-export([start_link/0,
         process_msg/1,
         send_buttons/3,
         request_location/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

-type message() :: {integer(), [byte()], integer(), [byte()]}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


-spec process_msg(message()) -> {reply, ok, []}.
process_msg(Msg) ->
    gen_server:call(?SERVER, Msg).

send_buttons(ChatID, Message, Shops) ->
    Buttons = inline_buttons(Shops),
    send_reply(ChatID, Message, Buttons).

request_location(ChatID, Message, _ShopID) ->
    send_reply(ChatID, Message, location_req()).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    log:info(?MFN, "Start processor"),
    self() ! do_init,
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(Request, _From, _State) ->
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
    end,
    {reply, ok, {}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(set_webhook, State) ->
    SetWebHookURL = web_hook_url(),
    log:info(?MFN, SetWebHookURL),
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(get, {SetWebHookURL, []}, [], []),
    log:info(?MFN, Body),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(do_init, _State) ->
    init_cowboy(),
    init_usertable(),
    gen_server:cast(?SERVER, set_webhook),
    {noreply, {}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
web_hook_url() ->
    {ok, BotUrl}     = application:get_env(?APPLICATION, bot_url),
    {ok, BotToken}   = application:get_env(?APPLICATION, token),
    {ok, WebHookURL} = application:get_env(?APPLICATION, wh_url),
    BotPath = binary:list_to_bin("/" ++ lists:last(string:tokens(BotToken, ":"))),
    Path = binary_to_list(BotPath),
    SetWebHookURL    = BotUrl ++ BotToken ++ "/setWebhook?url=" ++ WebHookURL ++ Path,
    SetWebHookURL.

-spec init_cowboy() -> ok.
init_cowboy() ->
    log:info(?MFN, "Start cowboy init"),
    {ok, Token} = application:get_env(?APPLICATION, token),
    log:info(?MFN, Token),
    {ok, IP}    = application:get_env(?APPLICATION, ip),
    log:info(?MFN, IP),
    {ok, Port}  = application:get_env(?APPLICATION, port),
    log:info(?MFN, Port),

    BotPath = binary:list_to_bin("/" ++ lists:last(string:tokens(Token, ":"))),
    log:info(?MFN, BotPath),

    Dispatch = cowboy_router:compile([
        {'_', [
            {BotPath, bot_cowboy_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(http,
        [
            {port, Port},
            {ip, IP}
        ],
        #{env => #{dispatch => Dispatch}}),
    log:info("Feldstaerke: Cowboy initialization complete."),
    ok.

%% @doc creates new ETS for user states
-spec init_usertable() ->  atom().
init_usertable() ->
    log:info(?MFN, "Start init usertable"),
    ets:new(usertable, [named_table, public, set]).

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
    feldstaerke_shopsm:show_shop_list(ChatId),
    ok;

reply_to_unauthorized(_UserId, _Username, ChatId, _) ->
    send_reply(ChatId, "Hello, fellow!\n\nIt is a proof of concept of fieldforce bot written in Erlang!\n\nBegin with /start").

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

handle_command(_UserId, _Username, ChatId, "/shops") ->
    feldstaerke_shopsm:show_shop_list(ChatId),
    ok;

handle_command(UserId, Username, ChatId, "/exit") ->
    ets:insert(usertable, {UserId, "unauthorized"}),
    send_reply(ChatId, Username ++ ", logout complete.");

handle_command(_UserId, _Username, _ChatId, "EnterShopID = " ++ ShopID0) ->
    ShopID = list_to_integer(ShopID0),
    feldstaerke_shopsm:request_user_location(ShopID),
    ok;

handle_command(_UserId, _Username, ChatId, _) ->
    send_reply(ChatId, "Oops! Don't know such command.\n\nUse /help to see what I can").