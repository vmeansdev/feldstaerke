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
    init_usertable(),
    init_cowboy(),
    set_web_hook(),
    feldstaerke_sup:start_link().

%%--------------------------------------------------------------------
-spec stop(_) -> ok.
stop(_State) ->
    log:info(?MFN, "app stopped"),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
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


set_web_hook() ->
    SetWebHookURL = web_hook_url(),
    log:info(?MFN, SetWebHookURL),
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(get, {SetWebHookURL, []}, [], []),
    log:info(?MFN, Body).

web_hook_url() ->
    {ok, BotUrl}     = application:get_env(?APPLICATION, bot_url),
    {ok, BotToken}   = application:get_env(?APPLICATION, token),
    {ok, WebHookURL} = application:get_env(?APPLICATION, wh_url),
    BotPath = binary:list_to_bin("/" ++ lists:last(string:tokens(BotToken, ":"))),
    Path = binary_to_list(BotPath),
    SetWebHookURL    = BotUrl ++ BotToken ++ "/setWebhook?url=" ++ WebHookURL ++ Path,
    SetWebHookURL.