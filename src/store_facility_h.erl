%% @doc A handler to store the package data in the database.
-module(store_facility_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
  
  #{<<"facility_uuid">> := Fac, <<"city">> := Cit} = jsx:decode(Data),

  Facility_uuid = binary_to_list(Fac),
  City = binary_to_list(Cit),

  store_facility:store(rr_distributor:get(store_facility_rr), Facility_uuid, City),

	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, "[\"done\"]", Req0),

	{ok, Req, Opts}.
