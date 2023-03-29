%%%=============================================================================
%%% @doc store_vehicle
%%% @end
%%%=============================================================================
-module(store_vehicle).
-behaviour(gen_server).

%% API
-export([start_link/3, stop/1, store/3]).
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
stop(Name) ->
  gen_server:call(Name, stop).

%%--------------------------------------------------------------------
%% @doc
%% Stores vehicle info.
%%
%% @end
%%--------------------------------------------------------------------
-spec store(Name :: atom(), Key :: binary(), Value :: binary()) -> term().
store(_, Key, _) when is_list(Key) == false; Key == [] ->
  {error, "Key must be a non-empty string."};
store(_, _, Value) when Value == []; not is_list(Value) ->
  {error, "Value must be a list of 3-tuples."};
store(_, _, [Head | _]) when not is_tuple(Head); tuple_size(Head) /=3 ->
  {error, "Value must be a list of 3-tuples."};
store(ServerRef, Key, Value) ->
  gen_server:call(ServerRef, {store, Key, Value}, infinity).

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
init(_Args) ->
  case riakc_pb_socket:start_link(env:riak_address(), 8087, [{connect_timeout, 1000}]) of
    {ok, Riak_pid} -> {ok, Riak_pid};
    _ -> {stop, link_failure}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: pid(), State :: term()) ->
                   {reply, term(), term()} |
                   {reply, term(), term(), integer()} |
                   {noreply, term()} |
                   {noreply, term(), integer()} |
                   {stop, term(), term(), integer()} |
                   {stop, term(), term()}.
handle_call(stop, _From, _State) ->
  {stop, normal, replace_stopped, down}; %% setting the server's internal state to down
handle_call({store, Key, Value}, _From, Riak_pid) ->
  Obj = riakc_obj:new(<<"vehicle">>, term_to_binary(Key), term_to_binary(Value)),

  case riakc_pb_socket:put(Riak_pid, Obj) of
       ok ->
      {reply, ok, Riak_pid};
      Error ->
      {stop, Error, Error, down}
  end.

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

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

% tests for the store_vehicle:handle_call callback for store/3.
handle_call_store_test_() ->
  {setup, fun setup/0, fun cleanup/1, fun instantiator/1}.

setup() ->
  meck:new(riakc_pb_socket, [passthrough]),
  ok.

cleanup(_) ->
  meck:unload().

instantiator(_) ->
  [
   handle_call_store_happy(),
   handle_call_store_put_error()
  ].

handle_call_store_happy() ->
  meck:expect(riakc_pb_socket, put, fun (_, _) -> ok end),

  Actual1 = store_vehicle:handle_call({store, "key1", [{"val1", "val", "val2"}]}, some_from_pid, some_riak_pid),

  Test1 = ?_assertEqual({reply, ok, some_riak_pid}, Actual1),

  [Test1].

handle_call_store_put_error() ->
  meck:expect(riakc_pb_socket, put, fun (_, _) -> {error, "error simulated"} end),

  Expected = {stop, {error, "error simulated"}, down},
  Actual1 = store_vehicle:handle_call({store, "key1", [{"val1", "val", "val2"}]}, some_from_pid, some_riak_pid),
  Test1 =
    {"handle case where riakc_pb_socket:put returns an error",
     ?_assertEqual(Expected, Actual1)},

  [Test1].

% tests for the store_vehicle:store/3 api function.
store_test_() ->
  [
   store_invalid_key(),
   store_invalid_value()
  ].

store_invalid_key() ->
  Expected = {error, "Key must be a non-empty string."},

  Actual1 = catch store_vehicle:store(pid, not_a_string, [{"val", "val", "val"}]),
  Actual2 = catch store_vehicle:store(pid, "", [{"val", "val", "val"}]),

  Test1 =
    {"store returns error tuple if an atom is given for the key",
     ?_assertEqual(Expected, Actual1)},

  Test2 = ?_assertEqual(Expected, Actual2),
  [Test1, Test2].

store_invalid_value() ->
  Expected = {error, "Value must be a list of 3-tuples."},

  Actual1 = catch store_vehicle:store(pid, "key", []),
  Actual2 = catch store_vehicle:store(pid, "key", not_a_list),
  Actual3 = catch store_vehicle:store(pid, "key", [1, 2, 3, 4]),
  Actual4 = catch store_vehicle:store(pid, "key", [{1, 2}]),

  Test1 = {"Value cannot be an empty list", ?_assertEqual(Expected, Actual1)},
  Test2 = {"Value must be a list of 3-tuples", ?_assertEqual(Expected, Actual2)},
  Test3 = {"Value must be a list of 3-tuples", ?_assertEqual(Expected, Actual3)},
  Test4 = {"Value must be a list of 3-tuples", ?_assertEqual(Expected, Actual4)},

  [Test1, Test2, Test3, Test4].

-endif.
