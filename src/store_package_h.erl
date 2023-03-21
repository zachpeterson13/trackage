%% @doc A handler to store the package data in the database.
-module(store_package_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
  
  #{<<"package_uuid">> := Pack, <<"holder_uuid">> := Holder, <<"time_stamp">> := Timestamp} = jsx:decode(Data),

  Package_uuid = binary_to_list(Pack),
  Holder_uuid = binary_to_list(Holder),

  io:fwrite("Package: ~p~nHolder: ~p~nTime: ~p~n", [Package_uuid, Holder_uuid, Timestamp]),

  % List = get_package:get(dsf, Package_uuid),
  % 
  % store_package:store(fdj, Package_uuid, [{Holder_uuid, Timestamp} | List]),

	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, "[\"done\"]", Req0),

	{ok, Req, Opts}.
