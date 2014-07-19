-module(cpanel_handler).
-behaviour(cowboy_loop_handler).
-export([init/3, handle/2, info/3, terminate/3]).
-export([dispatch/0]).
%% Prepped for JSON encoding
-define(OBJECT_KEYS, [
        {room,      {inf_room:get_labels()}},
        {user,      {inf_user:get_labels()}},
        {entity,    {inf_entity:get_labels()}},
        {race,      {inf_race:get_labels()}},
        {command,   {inf_command:get_labels()}}
    ]).

dispatch() ->
    Domain = application:get_env(ebid, cpanel_domain, '_'),
    Compiled = cowboy_router:compile([
        {Domain, [
                {"/draft-04/schema", cpanel_handler, [{redirect, <<"http://json-schema.org/draft-04/schema">>}]},
                {"/static/[...]", cowboy_static, [
                        {directory,     {priv_dir, infallible, [<<"static">>]}},
                        {mimetypes,     {fun mimetypes:path_to_mimes/2, default}}
                    ]},
                {"/cpanel/:type", cpanel_handler, [list]},
                {"/cpanel/:type/new", cpanel_handler, [edit]},
                {"/cpanel/:type/:id/edit", cpanel_handler, [edit]},
                {"/cpanel/", cpanel_handler, [home]},
                {'_', cpanel_handler, []}
            ]}
    ]),
    Compiled.

init(_Protocol, Req, [list]) ->
    {Type, Req2} = cowboy_req:binding(type, Req),
    Module = get_module(Type),
    {ok, Req2, {list, Type, Module}};
init(_Protocol, Req, [edit]) ->
    {Type, Req2} = cowboy_req:binding(type, Req),
    {ID, Req3} = cowboy_req:binding(id, Req2),
    {Method, Req4} = cowboy_req:method(Req3),
    Module = get_module(Type),
    Function = case Method of
        <<"GET">>   -> edit;
        <<"POST">>  -> write
    end,
    {ok, Req4, {Function, Type, Module, ID}};
init(_Protocol, Req, [new]) ->
    {Type, Req2} = cowboy_req:binding(type, Req),
    {Method, Req3} = cowboy_req:method(Req2),
    Module = get_module(Type),
    Function = case Method of
        <<"GET">> -> new;
        <<"POST">> -> write
    end,
    {ok, Req3, {Function, Type, Module}};
init(_Protocol, Req, [home]) ->
    {ok, Req, home};
init(_Protocol, Req, [{redirect, URL}]) ->
    {ok, Req, {redirect, URL}};
init(_Protocol, Req, _Else) ->
    {ok, Req, {error, 404}}.

handle(Req, {list, Type, Module}) ->
    Objects = Module:fetch_all(),
    Proplists = [ Module:to_proplist(Object) || Object <- Objects ],
    error_logger:error_report(Proplists),
    {ok, Content} = cpanel_list_dtl:render([
            {type,      Type},
            {objects,   Proplists}
        ]),
    {ok, Page} = cpanel_base_dtl:render([{content, Content}]),
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], Page, Req),
    {ok, Req2, complete};
handle(Req, {new, Type, Module}) ->
    VM = inf_world:vm(),
    {ok, Object} = erlv8_vm:run(VM, "{}"),
    {ok, Req2} = do_edit(Type, Module, Object, Req),
    {ok, Req2, complete};
handle(Req, {edit, Type, Module, ID}) ->
    Object = case Module:fetch(ID) of
        undefined ->
            {ok, Object} = Module:new(),
            Object;
        Object ->
            Object
    end,
    {ok, Req2} = do_edit(Type, Module, Object, Req),
    {ok, Req2, complete};
handle(Req, {write, Type, Module, ID}) ->
    {ok, Req2} = do_write(ID, Type, Module, Req),
    {ok, Req2, complete};
handle(Req, home) ->
    Content = "Welcome!",
    {ok, Rendered} = cpanel_base_dtl:render([{content, Content}]),
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], Rendered, Req),
    {ok, Req2, complete};
handle(Req, {redirect, URL}) ->
    {ok, Req2} = cowboy_req:reply(302, [{<<"location">>, URL},{<<"access-control-allow-origin">>,<<"*">>}], "", Req),
    {ok, Req2, complete};
handle(Req, {error, 404}) ->
    {ok, Req2} = cowboy_req:reply(404, Req),
    {ok, Req2, complete}.

info(_Info, Req, State) ->
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

do_edit(Type, Module, Object, Req) ->
    ObjectKeys      = jiffy:encode({?OBJECT_KEYS}),
    Proplist        = Module:to_proplist(Object),
    JSON            = Module:to_json(Object),
    {ok, Content}   = cpanel_edit_dtl:render([
            {type,          Type},
            {object,        Proplist},
            {object_json,   JSON},
            {object_keys,   ObjectKeys}
        ]),
    {ok, Page}      = cpanel_base_dtl:render([
            {content,       Content},
            {scripts,       ["js_composer_0.0.1.js","INFSchema.js"]}
        ]),
    {ok, Req2}      = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], Page, Req).

do_write(ID, Type, Module, Req) ->
    io:format("Writing ~p: ~p~n", [Type, ID]),
    {ok, Body, Req2} = cowboy_req:body_qs(Req),
    {<<"json">>, JSON} = lists:keyfind(<<"json">>, 1, Body),
    io:format("Updating...~n"),
    Module:update(ID, JSON),
    cowboy_req:reply(302, [{<<"location">>, [<<"/cpanel/">>, Type]}], "", Req2).

get_module(<<"setting_type">>)  -> inf_settings;
get_module(<<"setting">>)       -> inf_settings;
get_module(<<"entity">>)        -> inf_entity;
get_module(<<"user">>)          -> inf_user;
get_module(<<"room">>)          -> inf_room;
get_module(<<"command">>)       -> inf_command.
