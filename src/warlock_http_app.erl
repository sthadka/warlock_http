-module(warlock_http_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    % Start warlock
    application:start(compiler),
    application:start(syntax_tools),
    application:start(lager),
    application:start(eredis),
    application:start(ranch),

    application:start(war_util),
    application:start(war_db),
    application:start(war_consensus),
    application:start(war_server),

    warlock_http_sup:start_link().

stop(_State) ->
    ok.
