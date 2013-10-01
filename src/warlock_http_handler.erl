-module(warlock_http_handler).
-export([handle/2, handle_event/3]).

-include_lib("warlock_http.hrl").
-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

%% Home page
handle('GET', [], _Req) ->
    {200, [], <<"ok">>};

% join, repl, leave, remove, replace

handle('GET',[<<"join">>, Node], _Req) ->
    Ret = war_server_console:join([?b2l(Node)]),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle('GET',[<<"repl">>, Node], _Req) ->
    Ret = war_server_console:repl([?b2l(Node)]),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle('GET',[<<"leave">>], _Req) ->
    Ret = war_server_console:leave([]),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle('GET',[<<"remove">>, Node], _Req) ->
    Ret = war_server_console:remove([?b2l(Node)]),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle('GET',[<<"replace">>, TargetNode, SeedNode], _Req) ->
    Ret = war_server_console:replace([?b2l(TargetNode), ?b2l(SeedNode)]),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle('GET',[<<"set">>, Key, Value], _Req) ->
    Ret = war_server:x(clu, [set, Key, Value]),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle('GET',[<<"get">>, Key], _Req) ->
    Ret = war_server:x(loc, [get, Key]),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(request_error, Data, Args) ->
    error_logger:warning_msg("Request Error: ~p ~n ~p ~n", [Args, Data]);
handle_event(_Event, _Data, _Args) ->
    ok.
