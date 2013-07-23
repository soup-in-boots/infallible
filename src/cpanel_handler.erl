-module(cpanel_handler).
-export([init/3, handle/2, info/2, terminate/2]).
-export([dispatch/0]).
-record(state, {nothing}).

dispatch() ->
    Domain = application:get_env(ebid, cpanel_domain, '_'),
    Compiled = cowboy_router:compile([
        {Domain, [
                {"/static/[...]", cowboy_static, [
                        {directory,     {priv_dir, infallible, [<<"static">>]}},
                        {mimetypes,     {fun mimetypes:path_to_mimes/2, default}}
                    ]},
                {'_', cpanel_handler, []}
            ]}
    ]),
    error_logger:error_msg("Compiled Dispatch: ~p", [Compiled]),
    Compiled.

init(_Protocol, Req, _Opt) ->
    {ok, Req, undefined}.

info(Req, State) ->
    {ok, Req, State}.

handle(Req, State) ->
    {ok, Rendered} = cpanel_base:render([{content, "Welcome!"}]),
    {ok, Req2} = cowboy_http:reply(200, [{<<"content-type">>, <<"text/html">>}], Rendered, Req),
    {ok, Req2, State}.

terminate(Reason, State) ->
    ok.
