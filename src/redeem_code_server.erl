%%%-------------------------------------------------------------------
%%% @author xin
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 三月 2015 上午10:14
%%%-------------------------------------------------------------------
-module(redeem_code_server).
-author("xin").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([generate_redeem_code/1, make_a_new_code/0]).

%%test api
-export([generate_code_test/1, query_code_test/1, query_unused_code_test/0, use_code_test/1]).

-define(SERVER, ?MODULE).
-define(CODE_LENGTH, 8).

-record(state, {code_ets}).

-include("common.hrl").
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

  ?DEBUG("redeem code server init"),
  {ok, #state{code_ets = ets:new(code_ets,[named_table, set, {keypos, #redeem_code.r_code}])}}.

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
handle_call({create_redeem_code, Count}, _From, State) ->

  random:seed(now()),
  Codes = generate_redeem_code(Count),
  {reply, Codes, State};

handle_call({query_redeem_code, Code}, _From, State) ->

  RedeemCode = query_code(Code),
  {reply, RedeemCode, State};

handle_call({query_unused_redeem_code}, _From, State) ->

  UnusedCode = query_unused_code(),

  {reply, UnusedCode, State};

handle_call({use_redeem_code, Code}, _From, State) ->

  RedeemCode = use_code(Code),
  {reply, RedeemCode, State};

handle_call(_Requset, _From, State) ->

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
handle_cast(_Request, State) ->
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


%% Test functions
query_code_test(Code) ->

  ?DEBUG(gen_server:call(redeem_code_server,{query_redeem_code, Code})).

generate_code_test(Count) when is_integer(Count)->

  ?DEBUG(gen_server:call(redeem_code_server,{create_redeem_code, Count})).

query_unused_code_test() ->

  ?DEBUG(gen_server:call(redeem_code_server,{query_unused_redeem_code})).


use_code_test(Code) ->
  ?DEBUG(gen_server:call(redeem_code_server,{use_redeem_code, Code})).


query_unused_code() ->

  ets:match_object(code_ets, #redeem_code{is_use = false, _='_'}).

query_code(Code) ->

  case ets:match_object(code_ets, #redeem_code{r_code = Code, _='_'}) of

    [RedeemCode] ->  RedeemCode;

    [] -> false
  end.


use_code(Code) ->


  ets:update_element(code_ets, Code, [{#redeem_code.is_use, true},{#redeem_code.use_date, common:now()}]).


generate_redeem_code(0) ->
  [];

generate_redeem_code(Count) ->


  Code = make_a_new_code(),

  case length(Code) of

    ?CODE_LENGTH ->
      case insert_redeem_code_to_ets(Code) of

        true -> [Code | generate_redeem_code(Count - 1)];

        _   -> generate_redeem_code(Count)
      end;
    _   -> generate_redeem_code(Count)
  end.

insert_redeem_code_to_ets(Code) ->

  ets:insert_new(code_ets, #redeem_code{r_code = Code, create_date = common:now()}).

make_a_new_code() ->

  integer_to_list(round(random:uniform() * 1000000000000), 36).