%% @doc A handler to store the package data in the database.
-module(get_facility_h).

-export([init/2]).

init(Req0, Opts) ->
  {ok, Data, _} = cowboy_req:read_body(Req0),

  #{<<"facility_uuid">> := Facil} = jsx:decode(Data),

  Facility_uuid = binary_to_list(Facil),

  City =
    get_facility:get(
      rr_distributor:get(get_facility_rr), Facility_uuid),

  Map = #{<<"city">> => binary:list_to_bin(City)},

  Reply = jsx:encode(Map),

  Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/json">>}, Reply, Req0),

  {ok, Req, Opts}.
