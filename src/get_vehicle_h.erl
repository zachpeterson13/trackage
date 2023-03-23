%% @doc A handler to store the package data in the database.
-module(get_vehicle_h).

-export([init/2]).

init(Req0, Opts) ->
  {ok, Data, _} = cowboy_req:read_body(Req0),

  #{<<"vehicle_uuid">> := Veh} = jsx:decode(Data),

  Vehicle_uuid = binary_to_list(Veh),

  History_list =
    get_vehicle:get(
      rr_distributor:get(get_vehicle_rr), Vehicle_uuid),

  History_map =
    lists:map(fun({A, B, C}) ->
                 #{<<"location">> => #{<<"lat">> => A, <<"long">> => B}, <<"time_stamp">> => C}
              end,
              History_list),

  Reply = jsx:encode(#{<<"history">> => History_map}),

  Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/json">>}, Reply, Req0),

  {ok, Req, Opts}.
