-module(inf_room_sup).
-behaviour(supervisor).
-export([
        start_link/0,
        init/1
    ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {
            {one_for_all, 5, 5000},
            [
                {
                    inf_room_worker_sup,
                    {inf_room_worker_sup, start_link, []},
                    permanent,
                    60000,
                    supervisor,
                    [info_room_worker_sup]
                },
                {
                    inf_room_manager,
                    {inf_room_manager, start_link, []},
                    permanent,
                    1000,
                    worker,
                    [inf_room_manager]
                }
            ]
        }}.
