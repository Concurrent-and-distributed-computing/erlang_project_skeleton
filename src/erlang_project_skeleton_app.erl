%%%-------------------------------------------------------------------
%% @doc erlang_project_skeleton public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_project_skeleton_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    library:start(),
    Dispatch = cowboy_router:compile([{'_',
                                       [{"/health", health_route, []},
                                        {"/book", get_book_route, []},
                                        {"/books", post_book_route, []}]}]),
    {ok, _} = cowboy:start_clear(http,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch}}),
    % populate database
    erlang_project_skeleton_sup:start_link().

stop(_State) -> ok = cowboy:stop_listener(http).

%% internal functions

