-module(inf_command).
-export([
        execute/2,
        initialize_table/0,
        get_labels/0,
        to_proplist/1,
        to_json/1,
        from_json/1,
        fetch_all/0,
        fetch/1,
        update/2,
        write/1,
        new/0
    ]).
-include("infallible.hrl").

execute(Command, Context) ->
    execute(Command, [], Context).

execute(Command, Arguments, Context) ->
    case inf_command_manager:fetch(Command) of
        undefined -> throw(no_command);
        {Command, Function} -> Context:call(Context:get_value(Command), Arguments, Context)
    end.

initialize_table() ->
    mnesia:create_table(command, [
            {attributes, record_info(fields, command)},
            {disc_copies, [node()]}
        ]),
    import_default_commands().

import_default_commands() ->
    PrivDir = code:priv_dir(infallible),
    Path = filename:join([PrivDir, "commands"]),
    {ok, Files} = file:list_dir(Path),
    import_default_commands([ filename:join([Path, File]) || File <- Files ]).

import_default_commands([File|Files]) ->
    io:format("[~p][~s:~p] Load Command: ~s... ", [?MODULE, ?FILE, ?LINE, File]),
    Filename = filename:basename(File),
    case re:run(Filename, "(.+)\\.js$", [{capture, all_but_first, binary}]) of
        {match, [Command]} ->
            {ok, Content} = file:read_file(File),
            io:format("Loaded.~n"),
            write(#command{command = Command, permission = 0, execution = Content});
        nomatch ->
            io:format("Not loaded.~n")
    end,
    import_default_commands(Files);
import_default_commands([]) ->
    ok.


fetch_all() ->
    [ fetch(ID) || ID <- mnesia:dirty_all_keys(command) ].

fetch(undefined) ->
    undefined;
fetch(ID) ->
    RealID = unicode:characters_to_binary(ID),
    ?dirty_read(RealID, command).

write(Command = #command{}) ->
    mnesia:dirty_write(Command).

update(_Old, New = #command{}) ->
    write(New);
update(Old, New) ->
    io:format("Updating command from json...~n"),
    update(Old, from_json(New)).

new() -> {ok, #command{}}.

get_labels() ->
    Keys = mnesia:dirty_all_keys(command),
    lists:zip(Keys, Keys).

to_proplist(Command) when is_record(Command, command) ->
    lists:zip(record_info(fields, command), tl(tuple_to_list(Command))).

to_json(Command) ->
    Keys        = record_info(fields, command),
    Values      = tl(tuple_to_list(Command)),
    JSON        = lists:zipwith(fun to_json/2, Keys, Values),
    jiffy:encode({JSON}).

to_json(Key, Value) when is_list(Value) ->
    to_json(Key, unicode:characters_to_binary(Value));
to_json(Key, Value) ->
    {Key, Value}.

from_json(JSON) ->
    {Command} = jiffy:decode(JSON),
    io:format("Decoded: ~p~n", [Command]),
    Compiled = lists:foldl(fun from_json/2, #command{}, Command),
    io:format("Compiled: ~p~n", [Compiled]),
    Compiled.

from_json({RawK, V}, Command) when is_binary(RawK) ->
    K = binary_to_existing_atom(RawK, utf8),
    from_json({K, V}, Command);
from_json({K, V}, Command) ->
    Fields = record_info(fields, command),
    utils:assign_field(K, V, Fields, Command).

my_function() ->
    io:format("Testing~n").
