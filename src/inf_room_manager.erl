-module(inf_room_manager).
-behaviour(gen_server).
-export([
        %% API
        lookup/1,
        register_room/2,

        %% gen_server Callbacks
        start_link/0,
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2
    ]).
-include("infallible.hrl").

lookup(ID) ->
    case ets:lookup(room_refs, ID) of
        [] -> undefined;
        [{ID, Ref}] -> Ref
    end.

register_room(ID, Room) ->
    VM = inf_world:vm(),
    G = erlv8_vm:global(VM),
    G:set_value("room", Room),
    NewRoom = G:get_value("room"),
    case ets:insert_new(room_refs, {ID, NewRoom}) of
        true -> NewRoom;
        false -> lookup(ID)
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(room_refs, [
            public,
            named_table,
            {read_concurrency, true}
        ]),
    {ok, undefined}.

handle_call(_Call, _From, State) ->
    {reply, undefined, State}.

handle_cast(tick, State) ->
    Rooms = ets:tab2list(room_refs),
    lists:foreach(fun do_tick/1, Rooms),
    {noreply, State};
handle_cast(super_tick, State) ->
    Rooms = ets:tab2list(room_refs),
    lists:foreach(fun do_super_tick/1, Rooms),
    {noreply, State};
handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

do_tick({_ID, Room}) -> Room:call(Room:get_value("tick"), []).
do_super_tick({_ID, Room}) -> inf_room:write(Room).
