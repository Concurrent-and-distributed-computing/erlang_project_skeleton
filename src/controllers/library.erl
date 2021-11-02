-module(library).

-behaviour(gen_server).

-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).

-export([get/1, post/2, start/0]).

% These are all wrappers for calls to the server
start() ->
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [],
                          []).

get(ISBN) -> gen_server:call(?MODULE, {get, ISBN}).

post(ISBN, Book) ->
    gen_server:call(?MODULE, {post, ISBN, Book}).

% This is called when a connection is made to the server
init([]) ->
    Library = dict:new(),
    {ok, Library}.

% handle_call is invoked in response to gen_server:call
handle_call({post, ISBN, Book}, _From, Library) ->
    io:format("post ~p: ~p~n", [ISBN, Book]),
    Response = case dict:is_key(ISBN, Library) of
                   true ->
                       NewLibrary = Library,
                       {ok,
                        lists:flatten(io_lib:format(<<"{\"response\": \"book ~s already exists\"}">>,
                                                    [Book]))};
                   false ->
                       NewLibrary = dict:append(ISBN, Book, Library),
                       {ok,
                        lists:flatten(io_lib:format(<<"{\"response\": \"book ~s added\"}">>,
                                                    [Book]))}
               end,
    {reply, Response, NewLibrary};
handle_call({get, ISBN}, _From, Library) ->
    io:format("get ~p~n", [ISBN]),
    Response = case dict:is_key(ISBN, Library) of
                   true ->
                       {book,
                        lists:flatten(io_lib:format(<<"{\"response\": \"book ~s found\"}">>,
                                                    [lists:nth(1,
                                                               dict:fetch(ISBN,
                                                                          Library))]))};
                   false -> {book, <<"{\"response\": \"not found\"}">>}
               end,
    {reply, Response, Library}.

% We get compile warnings from gen_server unless we define these
handle_cast(_Message, Library) -> {noreply, Library}.

handle_info(_Message, Library) -> {noreply, Library}.

terminate(_Reason, _Library) -> ok.

code_change(_OldVersion, Library, _Extra) ->
    {ok, Library}.
