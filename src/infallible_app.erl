-module(infallible_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(cowboy),
    cowboy:start_listener(cowboy_listener, 256, 
        cowboy_tcp_transport, [{port, 1313}], 
        infallible_protocol, [{handler, login_handler}]
    ),
    infallible_sup:start_link().

stop(_State) ->
    ok.
