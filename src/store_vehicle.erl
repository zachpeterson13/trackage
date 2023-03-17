%%%=============================================================================
%%% @doc store_vehicle
%%% @end
%%%=============================================================================
-module(store_vehicle).
-behaviour(gen_server).

%% API
-export([start/3, stop/1, store/3]).
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
-spec start(atom(), atom(), atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Registration_type, Name, Args) ->
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
store(Name, Key, Value) ->
  gen_server:call(Name, {store, Key, Value}).

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
init([]) ->
  {ok, replace_with_riak_pid}.

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
handle_call({store, _Key, _Values}, _From, Riak_pid) ->
  {reply, stub, Riak_pid}.

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

store_test_() ->
  {setup, fun setup/0, fun cleanup/1, fun instantiator/1}.

setup() ->
  meck:new(riakc_pb_socket, [passthrough]),
  meck:expect(riakc_pb_socket, start_link, fun(_, _) -> {ok, fake_pid} end),
  {ok, Pid} = gen_server:start(?MODULE, [], []),
  ?assert(is_process_alive(Pid)),
  Pid.

cleanup(Pid) ->
  gen_server:stop(Pid),
  meck:unload(),
  ?assertEqual(false, is_process_alive(Pid)).

instantiator(Pid) ->
  [store_happy_path(Pid),
   store_invalid_key(Pid),
   store_invalid_value(Pid),
   store_put_error(Pid)].

store_happy_path(Pid) ->
  meck:expect(riakc_pb_socket, put, fun(_, _) -> ok end),
  
  Actual1 = store_vehicle:store(Pid, "key1", [{"val1", "val", "val2"}]),
  Test1 = ?_assertEqual(ok, Actual1),

  meck:delete(riakc_pb_socket, put, 2),

  [Test1].

store_invalid_key(Pid) ->
  Expected = {error, "Key must be a string."},

  Actual1 = store_vehicle:store(Pid, not_a_string, [{"val", "val", "val"}]),

  Test1 =
    {"store returns error tuple if an atom is given for the key",
     ?_assertEqual(Expected, Actual1)},

  [Test1].

store_invalid_value(Pid) ->
  Expected = {error, "Value must be a list of 3-tuples."},

  Actual1 = store_vehicle:store(Pid, "", []),
  Actual2 = store_vehicle:store(Pid, "", not_a_list),
  Actual3 = store_vehicle:store(Pid, "", [1, 2, 3, 4]),
  Actual4 = store_vehicle:store(Pid, "", [{1, 2}]),

  Test1 = {"Value cannot be an empty list", ?_assertEqual(Expected, Actual1)},
  Test2 = {"Value must be a list of 3-tuples", ?_assertEqual(Expected, Actual2)},
  Test3 = {"Value must be a list of 3-tuples", ?_assertEqual(Expected, Actual3)},
  Test4 = {"Value must be a list of 3-tuples", ?_assertEqual(Expected, Actual4)},

  [Test1, Test2, Test3, Test4].

store_put_error(Pid) ->
  meck:expect(riakc_pb_socket, put, fun (_, _) -> {error, "error simulated"} end),

  Expected = {error, "error simulated"},
  Actual1 = store_vehicle:store(Pid, "", ""),
  Test1 =
    {"handle case where riakc_pb_socket:put returns an error",
     ?_assertEqual(Expected, Actual1)},

  meck:delete(riakc_pb_socket, put, 2),

  [Test1].
-endif.
