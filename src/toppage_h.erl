%% @doc A handler to store the package data in the database.
-module(toppage_h).

-export([init/2]).

init(Req0, Opts) ->
  Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/json">>}, "[\"done\"]", Req0),

  {ok, Req, Opts}.
