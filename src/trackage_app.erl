%%%-------------------------------------------------------------------
%% @doc trackage public API
%% @end
%%%-------------------------------------------------------------------

-module(trackage_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch =
    cowboy_router:compile([{'_',
                            [{"/store_package_info", store_package_h, []},
                             {"/store_vehicle_info", store_vehicle_h, []},
                             {"/store_facility_info", store_facility_h, []},
                             {"/query_package_history", get_package_h, []},
                             {"/query_vehicle_history", get_vehicle_h, []},
                             {"/query_facility", get_facility_h, []}]}]),

  PrivDir = code:priv_dir(trackage),
  {ok, _} =
    cowboy:start_tls(https_listener,
                     [{port, 443},
                      {certfile, PrivDir ++ "/ssl/fullchain.pem"},
                      {keyfile, PrivDir ++ "/ssl/privkey.pem"}],
                     #{env => #{dispatch => Dispatch}}),

  trackage_sup:start_link().

stop(_State) ->
  ok.

%% internal functions
