-module(infallible_users).
-behaviour(gen_server).
-export([initialize_table/0]).
-export([check_login/2]).
-export([get_user/1,start_link/0,save_user/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-include("infallible.hrl").
-record(state, {
        logged_in       = []            :: [{string(), #user{}}]
    }).

initialize_table() ->
    mnesia:create_table(user, [
            {attributes, record_info(fields, user)},
            {disc_copies, [node()]}
        ]).

get_user(UserName) -> ?dirty_read(UserName, user).

check_login(undefined, _) ->
    false;
check_login(_User = #user{password = Correct}, Password) ->
    MD5 = infallible_utils:md5(Password),
    if
        MD5 == Correct ->
            true;
        true ->
            false
    end;
check_login(UserName, Password) ->
    check_login(get_user(UserName), Password).

save_user(User) when is_record(User, user) ->
    gen_server:cast(?MODULE, {save_user, User}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    initialize_table(),
    {ok, #state{}}.

handle_cast({save_user, User}, State) ->
    mnesia:dirty_write(User),
    {noreply, State};
handle_cast(_Data, State) ->
    {noreply, State}.

handle_call(_Data, _From, State) ->
    {noreply, State}.

handle_info(_Data, State) ->
    {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.
