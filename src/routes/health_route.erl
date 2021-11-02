-module(health_route).

-export([init/2]).

init(Req0, Opts) ->
    io:format("GET /health~n", []),
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"application/json">>},
                           <<"{\"health\": \"ok\"}">>,
                           Req0),
    {ok, Req, Opts}.
