%% @author Zach Peterson
%% @copyright 2022 Lee Barney licensed under the <a>
%%        rel="license"
%%        href="http://creativecommons.org/licenses/by/4.0/"
%%        target="_blank">
%%        Creative Commons Attribution 4.0 International License</a>
%%
%%
-module(store_package_server).

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
%% Stores package info.
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
   %% setting the server's internal state to down
  {stop, normal, replace_stopped, down};
handle_call({store, Package_uuid, Holder_uuid, Timestamp}, _From, Riak_pid) ->
  {reply, ok, Riak_pid}.

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
setup() ->
  ?debugMsg("setup"),
  {ok, Pid} = gen_server:start({local, tster}, ?MODULE, [], []),
  meck:new(riakc_pb_socket, [passthrough]),
  meck:expect(riakc_pb_socket, start_link, fun(_, _) -> [] end),
  meck:expect(riakc_pb_socket, put, fun(State, Request) -> [Request | State] end),
  Pid.

cleanup(Pid) ->
  ?debugMsg("cleanup"),
  gen_server:stop(Pid),
  meck:unload(),
  ?assertEqual(false, is_process_alive(Pid)).

server_is_alive(Pid) ->
  [
   ?_assertEqual(true, is_process_alive(Pid))
  ].

server_attempts_store_riakc_obj(Pid) ->
  store_package_server:store(Pid,"P001", "H001", 2_000),
  ?assert(meck:validate(riakc_pb_socket)),
  ExpectedObj = riakc_obj:new(<<"package">>, term_to_binary("P001"), term_to_binary(#{holder_uuid => "H001", timestamp => 2000})),
  Test1 = ?_assertEqual([ExpectedObj], sys:get_state(Pid)),

  [Test1].

store_package_server_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
   [
    fun server_is_alive/1,
    fun server_attempts_store_riakc_obj/1
   ]
  }.
-endif.
