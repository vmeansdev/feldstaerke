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

-behaviour(gen_server).

-include("../headers/feldstaerke.hrl").

%% API
-export([start_link/0,
         parse_msg/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

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

-spec parse_msg(binary()) -> ok.
parse_msg(Msg) ->
    gen_server:cast(?SERVER, Msg).

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
    log:info(?MFN, "Start incoming messages parser"),
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
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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
handle_cast(Msg, _) ->
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
            handle_cb_query(Inf);
        _ ->
            io:format("MsgType matched to *~n"),
            validate_message(true, 0)
    end,
    {noreply, {}}.

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
handle_info(_Info, State) ->
    {noreply, State}.

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

handle_cb_query(Body) ->
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