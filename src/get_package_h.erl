%% @doc A handler to store the package data in the database.
-module(get_package_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
  
  #{<<"package_uuid">> := Pack} = jsx:decode(Data),

  Package_uuid = binary_to_list(Pack),

  List = get_package:get(rr_distributor:get(get_package_rr), Package_uuid),

  Reply = binary_to_list(jsx:encode(#{<<"history">> => List})),

	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, Reply, Req0),

	{ok, Req, Opts}.
