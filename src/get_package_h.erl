%% @doc A handler to store the package data in the database.
-module(get_package_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
  
  #{<<"package_uuid">> := Pack} = jsx:decode(Data),

  Package_uuid = binary_to_list(Pack),

  History_list = get_package:get(rr_distributor:get(get_package_rr), Package_uuid),

  History_map = lists:map(fun({A, B}) -> #{<<"holder_uuid">> => term_to_binary(A), <<"time_stamp">> => B} end, History_list),

  Reply = jsx:encode(#{<<"history">> => History_map}),

	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, Reply, Req0),

	{ok, Req, Opts}.
