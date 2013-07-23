-module(inf_room_worker_sup).
-behaviour(supervisor).
-export([
        %% API
        start_room/1,

        %% supervisor Callbacks
        start_link/0,
        init/1
    ]).

start_room(Room) ->
    supervisor:start_child(?MODULE, [Room]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {
            {simple_one_for_one, 5, 5000},
            [
                {
                    inf_room_worker,
                    {inf_room, start_link, []},
                    permanent,
                    5000,
                    worker,
                    [inf_room]
                }
            ]
        }}.
