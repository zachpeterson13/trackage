%%%=============================================================================
%%% @doc rr_distributor
%%% @end
%%%=============================================================================
-module(rr_distributor).

-behaviour(gen_server).

%% API
-export([start_link/3, stop/1, get/1, add/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start the server with the given Registration_type, Name, and Args
%% Registration_type can be `local` or `global`
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom(), atom(), atom()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Registration_type, Name, Args) ->
  gen_server:start_link({Registration_type, Name}, ?MODULE, Args, []).

%%--------------------------------------------------------------------
%% @doc
%% Stop the server gracefully.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(Name :: atom()) -> {ok} | {error, term()}.
stop(ServerRef) ->
  gen_server:call(ServerRef, stop).

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @end
%%--------------------------------------------------------------------
get(ServerRef) ->
  gen_server:call(ServerRef, get_next).

%%--------------------------------------------------------------------
%% @doc
%% 
%%
%% @end
%%--------------------------------------------------------------------
add(ServerRef, To_add) ->
  gen_server:call(ServerRef, {add, To_add}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init(term()) -> {ok, term()} | {ok, term(), number()} | ignore | {stop, term()}.
init(Args) ->
  {ok, Args}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------%------------------------------------------------
-spec handle_call(Request :: term(), From :: pid(), State :: term()) ->
                   {reply, term(), term()} |
                   {reply, term(), term(), integer()} |
                   {noreply, term()} |
                   {noreply, term(), integer()} |
                   {stop, term(), term(), integer()} |
                   {stop, term(), term()}.
handle_call(stop, _From, _State) ->
 {stop, normal, replace_stopped, down}; %% setting the server's internal state to down
handle_call({add, New_pid}, _From, State) ->
  {reply, ok, [New_pid | State]};
handle_call(get_next, _From, [Head | Tail]) ->
  {reply, Head, Tail ++ [Head]}.

%%--------------------------------------------------------------------
%% @private

%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg :: term(), State :: term()) ->
                   {noreply, term()} | {noreply, term(), integer()} | {stop, term(), term()}.
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
%%--------------------------------------------------------------------

-spec handle_info(Info :: term(), State :: term()) ->
                   {noreply, term()} | {noreply, term(), integer()} | {stop, term(), term()}.
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
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), term()) -> term().
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), term(), term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_next_test_() ->
  Actual1 = rr_distributor:handle_call(get_next, ok, [server1, server2, server3, server4]),
  Actual2 = rr_distributor:handle_call(get_next, ok, [server2, server3, server4]),
  Actual3 = rr_distributor:handle_call(get_next, ok, [server3, server4]),
  Actual4 = rr_distributor:handle_call(get_next, ok, [server4]),


  Test1 = ?_assertEqual({reply, server1, [server2, server3, server4, server1]}, Actual1),
  Test2 = ?_assertEqual({reply, server2, [server3, server4, server2]}, Actual2),
  Test3 = ?_assertEqual({reply, server3, [server4, server3]}, Actual3),
  Test4 = ?_assertEqual({reply, server4, [server4]}, Actual4),

  [Test1, Test2, Test3, Test4].

add_test_() ->
  Actual1 = rr_distributor:handle_call({add, new_pid1}, from, [server1, server2, server3, server4]),
  Actual2 = rr_distributor:handle_call({add, new_pid2}, from, [new_pid1, server1, server2, server3, server4]),
  Actual3 = rr_distributor:handle_call({add, new_pid3}, from, [new_pid2, new_pid1, server1, server2, server3, server4]),
  Actual4 = rr_distributor:handle_call({add, new_pid4}, from, [new_pid3, new_pid2, new_pid1, server1, server2, server3, server4]),

  Test1 = ?_assertEqual({reply, ok, [new_pid1, server1, server2, server3, server4]}, Actual1),
  Test2 = ?_assertEqual({reply, ok, [new_pid2, new_pid1, server1, server2, server3, server4]}, Actual2),
  Test3 = ?_assertEqual({reply, ok, [new_pid3, new_pid2, new_pid1, server1, server2, server3, server4]}, Actual3),
  Test4 = ?_assertEqual({reply, ok, [new_pid4, new_pid3, new_pid2, new_pid1, server1, server2, server3, server4]}, Actual4),

  [Test1, Test2, Test3, Test4].

-endif.
