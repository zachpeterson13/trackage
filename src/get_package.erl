%%%=============================================================================
%%% @doc get_package
%%% @end
%%%=============================================================================
-module(get_package).

-behaviour(gen_server).

%% API
-export([start/3, stop/1, get/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts a server using this module and registers the server using
%% the name given.
%% Registration_type can be local or global.
%%
%% Args is a list containing any data to be passed to the gen_server's
%% init function.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom(), atom(), atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Registration_type, Name, Args) ->
  gen_server:start_link({Registration_type, Name}, ?MODULE, Args, []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(Name :: atom()) -> {ok} | {error, term()}.
stop(Name) ->
  gen_server:call(Name, stop).

get(Name, Pkg_uuid) ->
  gen_server:call(Name, {get, Pkg_uuid}).

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
  {ok, replace_up}.

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
handle_call({get, _Key}, _From, Riak_pid) ->
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

% tests for the get_package:handle_call callback for get/3.
handle_call_get_test_() ->
  {setup, fun setup/0, fun cleanup/1, fun instantiator/1}.

setup() ->
  meck:new(riakc_pb_socket, [passthrough]),
  meck:new(riakc_obj, [passthrough]),
  ok.

cleanup(_) ->
  meck:unload().

instantiator(_) ->
  [
   handle_call_get_happy(),
   handle_call_get_fetch_error()
  ].

handle_call_get_happy() ->
  meck:expect(riakc_pb_socket, get, fun(_, _, _) -> {ok, ok} end),
  meck:expect(riakc_obj,
  get_value,
  fun(_) ->
     <<131, 108, 0, 0, 0, 1, 104, 2, 107, 0, 9, 104, 111, 108, 100, 101, 114, 32, 105, 100, 98, 0, 53, 56, 179, 106>>
  end),

  Actual1 = get_package:handle_call({get, "key1"}, some_from_pid, some_riak_pid),

  Test1 = ?_assert(is_list(Actual1)),
  Test2 = ?_assertEqual([{"holder id",3487923}], Actual1),


  [Test1, Test2].

handle_call_get_fetch_error() ->
  meck:expect(riakc_pb_socket, get, fun(_, _, _) -> {error, "Error message"} end),

  Expected = {error, "Error message"},

  Actual1 = get_package:handle_call({get, "key1"}, some_from_pid, some_riak_pid),

  Test1 =
    {"Handle case if riakc_pb_socket:get returns an error.",
     ?_assertEqual(Expected, Actual1)},

  [Test1].

% tests for the get_package:get/2 api function.
get_test_() ->
  [
   get_invalid_key()
  ].

get_invalid_key() ->
  Expected = {error, "Key must be a non-empty string."},

  Actual1 = catch get_package:get(pid, not_a_string),
  Actual2 = catch get_package:get(pid, ""),

  Test1 = ?_assertEqual(Expected, Actual1),
  Test2 = ?_assertEqual(Expected, Actual2),

  [Test1, Test2].

-endif.
