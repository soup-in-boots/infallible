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

register_entity(ID, Object) ->
    case ets:insert_new(entity_refs, {ID, Object}) of
        false ->
            io:format("Insert new failed...~n"),
            lookup(ID);
        true ->
            io:format("Success...~n"),
            Object
    end.

lookup(ID) ->
    case ets:lookup(entity_refs, ID) of
        [] -> undefined;
        [{ID, Object}] -> Object
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

handle_cast(super_tick, State) ->
    Entities = ets:tab2list(entity_refs),
    lists:foreach(fun do_super_tick/1, Entities),
    {noreply, State};
handle_cast(tick, State) ->
    Entities = ets:tab2list(entity_refs),
    lists:foreach(fun do_tick/1, Entities),
    {noreply, State};
handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info({timeout, _TRef, Entity}, State) ->
    inf_entity:unload(Entity),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

do_tick({_ID, Obj}) ->
    Obj:call(Obj:get_value("tick", [])).

do_super_tick({_ID, Obj}) ->
    Obj:call(Obj:get_value("save", [])).
