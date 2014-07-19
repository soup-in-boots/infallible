-module(inf_command_manager).
-behaviour(gen_server).
-export([
        fetch/1,
        start_link/0,

        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2
    ]).
-include("infallible.hrl").
-define(RELOAD_INTERVAL, 60000).

fetch(ID) ->
    case ets:lookup(command_funs, ID) of
        [] -> undefined;
        [Fun] -> Fun
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    VM = inf_world:vm(),
    ets:new(command_funs, [named_table, set, protected]),
    load(VM),
    {ok, VM}.

handle_call(_Call, _From, State) ->
    {reply, undefined, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info({timeout, _Ref, reload}, VM) ->
    load(VM),
    {noreply, VM};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

load(VM) ->
    prep_reload(),
    G = erlv8_vm:global(VM),
    Commands = load_commands(VM, inf_command:fetch_all()),
    Interface = erlv8_object:new(Commands),
    G:set_value("Commands", Interface),
    ets:insert(command_funs, Commands).

load_commands(VM, [Command|Commands]) ->
    [load_command(VM, Command)|load_commands(VM, Commands)];
load_commands(_VM, []) ->
    [].

load_command(VM, _ = #command{command = Command, execution = Function}) ->
    LCommand = unicode:characters_to_list(Command),
    case erlv8_vm:run(VM, iolist_to_binary(["f = ", Function])) of
        {ok, Fun} ->
            io:format("[~p][~p:~p] Loaded Command: ~p~n", [?MODULE, ?FILE, ?LINE, Command]),
            {LCommand, Fun};
        {throw, Error} ->
            io:format("[~p][~p:~p] Could not load ~p: ~p~n", [?MODULE, ?FILE, ?LINE, Command, Error:proplist()]),
            {LCommand, undefined};
        Other ->
            io:format("[~p][~p:~p] Could not load ~p: ~p~n", [?MODULE, ?FILE, ?LINE, Command, Other]),
            {LCommand, undefined}
    end.

prep_reload() ->
    erlang:start_timer(?RELOAD_INTERVAL, self(), reload).
