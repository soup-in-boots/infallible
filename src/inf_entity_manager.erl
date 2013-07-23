-module(inf_entity_manager).
-export([
        register_entity/2,
        lookup/1,

        start_link/0,
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2
    ]).
-include("infallible.hrl").

register_entity(ID, Ref) ->
    case ets:insert_new(entity_refs, {ID, Ref}) of
        false -> lookup(ID);
        true -> Ref
    end.

lookup(ID) when is_list(ID) ->
    case ets:lookup(entity_refs, ID) of
        [] -> undefined;
        [{ID, Ref}] -> Ref
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(entity_refs, [
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
