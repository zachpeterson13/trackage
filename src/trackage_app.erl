%%%-------------------------------------------------------------------
%% @doc trackage public API
%% @end
%%%-------------------------------------------------------------------

-module(trackage_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    trackage_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
