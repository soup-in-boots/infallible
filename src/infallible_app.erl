-module(infallible_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-define(DISPATCH, {
        {'_', [
                {[<<"cpanel">>, '...'], cpanel_handler, []}
            ]}
    }).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(cowboy),
    application:start(mnesia),
    cowboy:start_listener(cowboy_listener, 32, 
        cowboy_tcp_transport, [{port, 1313}], 
        infallible_protocol, [{handler, login_handler}]
    ),
    cowboy:start_listener(cowboy_listener, 32,
        cowboy_tcp_transport, [{port, 80}],
        cowboy_http_protocol, [{dispatch, ?DISPATCH}]
    ),
    infallible_sup:start_link().

stop(_State) ->
    ok.
