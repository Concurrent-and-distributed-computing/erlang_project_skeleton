-module(get_book_route).

-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    #{isbn := ISBN} = cowboy_req:match_qs([{isbn,
                                            [],
                                            undefined}],
                                          Req0),
    Req = get(Method, ISBN, Req0),
    {ok, Req, Opts}.

get(<<"GET">>, undefined, Req) ->
    cowboy_req:reply(400,
                     #{},
                     <<"Missing echo parameter.">>,
                     Req);
get(<<"GET">>, ISBN, Req) ->
    {book, Book} = library:get(ISBN),
    {ok, CorsReq} = cors(Req),
    cowboy_req:reply(200,
                     #{<<"content-type">> => <<"application/json">>},
                     Book,
                     CorsReq);
get(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

cors(Req) ->
    Req1 =
        cowboy_req:set_resp_header(<<"access-control-allow-methods">>,
                                   <<"GET, OPTIONS">>,
                                   Req),
    Req2 =
        cowboy_req:set_resp_header(<<"access-control-allow-origin">>,
                                   <<"*">>,
                                   Req1),
    {ok, Req2}.
