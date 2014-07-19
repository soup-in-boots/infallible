-module(inf_reference).

-export([
        new/1,
        read/1,
        write/2,
        transform/2
     ]).

-on_load(init/0).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

new(Term) ->
    {ok, Ref} = do_new(Term),
    {ok, {'$inf_reference', make_ref(), Ref}}.

do_new(_Term) ->
    ?nif_stub.

read_lock(_Ref) ->
    ?nif_stub.

read_unlock(_Ref) ->
    ?nif_stub.

write_lock(_Ref) ->
    ?nif_stub.

write_unlock(_Ref) ->
    ?nif_stub.

read({'$inf_reference', _ERef, Ref}) ->
    read_lock(Ref),
    Res = (catch do_read(Ref)),
    read_unlock(Ref),
    {ok, _} = Res.

do_read(_Ref) ->
    ?nif_stub.

write({'$inf_reference', _ERef, Ref}, Term) ->
    write_lock(Ref),
    Res = (catch do_write(Ref, Term)),
    write_unlock(Ref),
    ok = Res.

do_write(_Ref, _Term) ->
    ?nif_stub.

transform({'$inf_reference', _ERef, Ref}, Fun) ->
    write_lock(Ref),
    Res = (catch do_transform(Ref, Fun)),
    write_unlock(Ref),
    ok = Res.

do_transform(Ref, Fun) ->
    {ok, Term} = do_read(Ref),
    NewTerm = Fun(Term),
    do_write(Ref, NewTerm).

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, Ref} = new(),
    ?assertEqual(ok, myfunction(Ref)).

-endif.
