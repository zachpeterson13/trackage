%%%=============================================================================
%%% @doc get_facility
%%% @end
%%%=============================================================================
-module(get_facility).
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

get(Name, Facility_uuid) ->
  gen_server:call(Name, {get, Facility_uuid}).

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

get_test_() ->
  {setup, fun setup/0, fun cleanup/1, fun instantiator/1}.

setup() ->
  % mock riakc_pb_socket:start_link
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
  [get_happy_path(Pid), get_invalid_key(Pid), get_fetch_error(Pid)].

get_happy_path(Pid) ->
  meck:expect(riakc_pb_socket, get, fun(_, _, _) -> {ok, ok} end),
  meck:expect(riakc_obj,
              get_value,
              fun(_) ->
                  <<131,107,0,13,78,101,119,32,89,111,114,107,32,67,105,116,121>>
              end),

  Actual1 = get_facility:get(Pid, "good key"),
  Test1 = ?_assertEqual("New York City", Actual1),

  meck:delete(riakc_pb_socket, get, 3),
  meck:delete(riakc_obj, get_value, 1),

  [Test1].

get_invalid_key(Pid) ->
  Expected = {error, "Key must be a string"},
  Actual1 = get_facility:get(Pid, not_a_string),
  Test1 = ?_assertEqual(Expected, Actual1),
  [Test1].

get_fetch_error(Pid) ->
  meck:expect(riakc_pb_socket, get, fun(_, _, _) -> {error, "Error message"} end),

  Expected = {error, "Error message"},
  Actual1 = get_facility:get(Pid, ""),
  Test1 =
    {"Handle case if riakc_pb_socket:get returns an error.",
     ?_assertEqual(Expected, Actual1)},

  meck:delete(riakc_pb_socket, get, 3),

  [Test1].

-endif.
