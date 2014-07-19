-module(inf_world).
-behaviour(gen_server).
-export([
        vm/0,
        make_vm/0,
        set_globals/1,

        initialize_table/0,
        fetch_all/0,
        fetch_all/1,
        fetch/1,
        write/1,

        start_link/0,

        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2,
        do_call/3
    ]).
-define(WORLD_MODULES, [
        "Utils.js",
        "Entity.js",
        "Room.js"
    ]).
-define(SYSTEM_MODULES, [
        inf_entity,
        inf_room,
        inf_world
    ]).
-include("infallible.hrl").
-include_lib("erlv8/include/erlv8.hrl").
-compile({no_auto_import, [load_module/2]}).

initialize_table() ->
    mnesia:create_table(object, [
            {attributes, record_info(fields, object)},
            {disc_copies, [node()]},
            {index, [#object.type]}
        ]).

fetch_all() ->
    [ fetch(ID) || ID <- mnesia:dirty_all_keys(object) ].

fetch_all(Type) ->
    mnesia:dirty_index_read(object, Type, #object.type).

fetch(ID) ->
    case mnesia:dirty_read(object, ID) of
        [] -> undefined;
        [Object] -> Object
    end.

lookup(ID) ->
    lookup(ID, [fun inf_entity_manager:lookup/1, fun inf_room_manager:lookup/1]).

lookup(ID, []) -> undefined;
lookup(ID, [F|Rest]) ->
    case F(ID) of
        undefined -> lookup(ID, Rest);
        Obj -> Obj
    end.

write(Object = #object{}) ->
    mnesia:dirty_write(Object).

vm() -> gen_server:call(?MODULE, vm).

make_vm() -> 
    {ok, VM} = erlv8_vm:start(),
    load_modules(VM),
    VM.

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    VM = make_vm(),
    {ok, VM}.

handle_call(vm, _From, VM) ->
    {reply, VM, VM};
handle_call(_Call, _From, VM) ->
    {reply, undefined, VM}.

handle_cast(_Cast, VM) ->
    {noreply, VM}.

handle_info({timeout, _Ref, {interval, Obj, Interval, Function}}, VM) ->
    Function:call(),
    TRef = erlang:start_timer(Interval, ?MODULE, {interval, Obj, Interval, Function}),
    Obj:set_value("ref", TRef),
    {noreply, VM};
handle_info({timeout, _Ref, {timeout, Function}}, VM) ->
    Function:call(),
    {noreply, VM};
handle_info(Info, VM) ->
    io:format("[~p][~p:~p] Unhandled Info: ~p~n", [?MODULE, ?FILE, ?LINE, Info]),
    {noreply, VM}.

code_change(_OldVsn, VM, _Extra) ->
    {ok, VM}.

terminate(_Reason, _VM) ->
    ok.

load_modules(VM) ->
    G = erlv8_vm:global(VM),
    G:set_value("System", system_module()),
    Path = filename:join([code:priv_dir(infallible), "world"]),
    lists:foreach(fun(Module) -> load_module(VM, filename:join([Path, Module])) end, ?WORLD_MODULES),
    lists:foreach(fun(Module) -> Module:set_globals(VM) end, ?SYSTEM_MODULES).

load_module(VM, File) ->
    io:format("Loading File [~s]... ", [File]),
    {ok, Module} = file:read_file(File),
    case erlv8_vm:run(VM, Module) of
        {ok, _Res} -> io:format("Success.~n");
        {throw, Error} when is_record(Error, erlv8_object) -> io:format("Failure: ~p~n", [Error:proplist()]);
        Error -> io:format("Failure: ~p~n", [Error])
    end.

set_globals(VM) ->
    G = erlv8_vm:global(VM),
    lists:foreach(fun({K, V}) -> G:set_value(K, V) end, [
            {"setTimeout",  fun(Context, [Function, Timeout]) ->
                        VM = Context:vm(),
                        Obj = erlv8_object:new([], VM),
                        TRef = erlang:start_timer(Timeout, ?MODULE, {timeout, Function}),
                        io:format("TRef: ~p~n", [TRef]),
                        Obj:set_value("ref", TRef),
                        Obj
                end},
            {"setInterval", fun(Context, [Function, Interval]) -> 
                        VM = Context:vm(),
                        Obj = erlv8_object:new([], VM),
                        TRef = erlang:start_timer(Interval, ?MODULE, {interval, Obj, Interval, Function}),
                        io:format("TRef: ~p~n", [TRef]),
                        Obj:set_value("ref", TRef),
                        Obj
                end},
            {"clearTimeout", fun(_Context, [Obj]) -> 
                        case erlang:cancel_timer(Obj:get_value("ref")) of
                            {ok, cancel} -> true;
                            _  -> false
                        end
                    end}
        ]).

system_module() ->
    io:format("Setting System globals~n"),
    erlv8_object:new([
            {"notify", fun sys_notify/2},
            {"save", fun sys_save/2},
            {"fetch", fun sys_load/2},
            {"route", fun sys_route/2},
            {"log", fun sys_log/2},
            {"getSetting", fun sys_get_setting/2}
    ]).

sys_notify(_Context, [Pid|Message]) ->
    Pid ! {system, Message}.

sys_save(Context, [Type, ID, JSON]) ->
    write(#object{id = ID, type = Type, json = JSON}).

sys_load(Context, [Type, ID]) ->
    case Type of
        <<"Room">> -> 
            inf_room:fetch(ID);
        <<"Entity">> ->
            inf_entity:fetch(ID)
    end.

sys_route(Context, [ID]) ->
    io:format("Route has been called...~n"),
    Obj         = lookup(ID),
    TargetVM    = Obj:vm(),
    CurrVM      = Context:vm(),
    case TargetVM of
        CurrVM -> 
            Obj;
        TargetVM ->
            make_reference(TargetVM, Obj)
    end.
sys_log(_Context, Args) ->
    io:format("[System.log] ~s~n", [ [ io_lib:format("~p", [V]) || V <- Args ] ]).

sys_get_setting(_Context, [Setting]) ->
    case inf_settings:fetch(Setting) of
        undefined -> undefined;
        _ = #setting{value = Value} -> Value
    end.

do_call(F, T, A) ->
    io:format("[~p][~p:~p] Calling: ~p :: ~p :: ~p~n", [?MODULE, ?FILE, ?LINE, F, T, A]),
    FO  = F:object(),
    FVM = FO:vm(),
    TVM = T:vm(),
    case FVM of
        TVM -> 
            io:format("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~n"),
            io:format("Making reference...~n"),
            F:call(T, A);
        FVM -> 
            io:format("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~n"),
            io:format("Making reference...~n"),
            Ref = make_reference(FVM, T),
            io:format("[~p][~p:~p] Made reference: ~p", [?MODULE, ?FILE, ?LINE, Ref]), %, Ref:proplist()]),
            F:call(Ref, A)
    end.

make_reference(VM, Target) when is_record(Target, erlv8_object) ->
    io:format("Object reference...~n"),
    Ref = erlv8_object:new([], VM),
    io:format("Reference properties...~n"),
    lists:foreach(fun({K, V}) -> io:format("~p: ~p | ", [K, V]), make_reference(VM, Target, Ref, K, V) end, Target:proplist()),
    io:format("Reference made...~n"),
    Ref;
make_reference(VM, Target) when is_record(Target, erlv8_array) ->
    io:format("Array reference...~n"),
    Ref             = erlv8_array:new([], VM),
    RefObject       = Ref:object(),
    TargetObject    = Target:object(),
    io:format("Array elements...~n"),
    lists:foreach(fun(V) -> Ref:push(make_reference(VM, V)) end, Target:list()),
    io:format("Array properties...~n"),
    lists:foreach(fun({K, V}) -> make_reference(VM, TargetObject, RefObject, K, V) end, TargetObject:proplist()),
    Ref;
make_reference(VM, Target) when is_record(Target, erlv8_fun) ->
    io:format("Function reference...~n"),
    TargetVM        = Target:vm(),
    TargetObject    = Target:object(),
    io:format("Function body...~n"),
    Ref             = erlv8_fun:new(fun(Context, Args) ->
                This    = Context:this(),
                ThisRef = make_reference(TargetVM, This),
                Target:call(ThisRef, [ make_reference(TargetVM, V) || V <- Args ])
        end),
    RefObject       = Ref:object(),
    io:format("Function properties...~n"),
    lists:foreach(fun({K, V}) -> io:format("~p: ", [K]), make_reference(VM, TargetObject, RefObject, K, V) end, TargetObject:proplist()),
    Ref;
make_reference(VM, Value) ->
    io:format("Bare value[1]...~n"),
    Value.

make_reference(VM, Target, Ref, K, V) when is_record(V, erlv8_object) ->
    io:format("Object value...~n"),
    Ref:set_value(K, make_reference(VM, V));
make_reference(VM, Target, Ref, K, V) when is_record(V, erlv8_array) ->
    io:format("Array value...~n"),
    O = V:object(),
    lists:foreach(fun({K, V}) -> io:format("[make_reference::array] ~p: ~p~n", [K, V]) end, O:proplist()),
    O:set_value(K, make_reference(VM, O));
make_reference(VM, Target, Ref, K, V) when is_record(V, erlv8_fun) ->
    io:format("Function value..."),
    Ref:set_value(K, fun(Context, Args) -> Target:call(Target:get_value(K), [ make_reference(Context:vm(), V) || V <- Args ]) end),
    io:format("set.~n"),
    Ref;
make_reference(VM, Target, Ref, K, V) ->
    io:format("Bare value[2]...~n"),
    Get = erlv8_fun:new(fun(Context, _Args) -> make_reference(Context:vm(), Target:get_value(K)) end, VM), 
    io:format("Made get...~n"),
    Set = erlv8_fun:new(fun(Context, [V]) -> Target:set_value(K, make_reference(Context:vm(), V)) end, VM),
    io:format("Setting accessor: ~p~n", [Ref]),
    Ref:set_accessor(K, Get, Set),
    io:format("Set accessor...~n"),
    Ref.
