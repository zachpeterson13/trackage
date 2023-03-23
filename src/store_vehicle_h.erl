%% @doc A handler to store the package data in the database.
-module(store_vehicle_h).

-export([init/2]).

init(Req0, Opts) ->
  {ok, Data, _} = cowboy_req:read_body(Req0),

  #{<<"vehicle_uuid">> := Veh,
    <<"location">> := Location,
    <<"time_stamp">> := Timestamp} =
    jsx:decode(Data),

  Vehicle_uuid = binary_to_list(Veh),
  #{<<"lat">> := Lat, <<"long">> := Long} = Location,

  List =
    get_vehicle:get(
      rr_distributor:get(get_vehicle_rr), Vehicle_uuid),

  store_vehicle:store(
    rr_distributor:get(store_vehicle_rr), Vehicle_uuid, [{Lat, Long, Timestamp} | List]),

  Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/json">>}, "[\"done\"]", Req0),

  {ok, Req, Opts}.
