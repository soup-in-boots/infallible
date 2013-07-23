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

lookup(ID) when is_list(ID) ->
    case ets:lookup(room_refs, ID) of
        [] -> undefined;
        [{ID, Ref}] -> Ref
    end.

register_room(ID, Ref) ->
    case ets:insert_new(room_refs, {ID, Ref}) of
        true -> Ref;
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

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
