
-module(inf_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RoomSpec    = ?CHILD(inf_room_manager, worker),
    EntitySpec  = ?CHILD(inf_entity_manager, worker),
    ClientSpec  = ranch:child_spec(client_listener, 32, 
        ranch_tcp, [{port, 1313}], 
        inf_protocol, [{handler, login_handler}]),
    PanelSpec   = ranch:child_spec(panel_http, 32,
        ranch_tcp, [{port, 80}],
        cowboy_http_protocol, [{dispatch, cpanel_handler:dispatch()}]),
    {ok, {
            {one_for_one, 5, 10}, 
            [
                EntitySpec,
                RoomSpec,
                ClientSpec,
                PanelSpec
            ]
        }}.

