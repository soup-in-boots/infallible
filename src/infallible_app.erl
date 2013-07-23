-module(infallible_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(OBJECT_MODULES, [
        inf_user,
        inf_entity,
        inf_race,
        inf_room
    ]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(ranch),
    application:start(cowboy),
    application:start(mnesia),
    initialize_tables(),
    inf_sup:start_link().

stop(_State) ->
    ok.

initialize_tables() ->
    lists:foreach(fun initialize_table/1, ?OBJECT_MODULES).

initialize_table(Module) ->
    Result = (catch Module:initialize_table()),
    error_logger:info_msg("[initialize_table] ~p || ~p", [Module, Result]).
