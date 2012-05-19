-module(login_handler).
-export([init/1, handle_data/2, handle_info/2, terminate/2]).
-include("infallible.hrl").
    
init(_) ->
    {send_message, "Welcome to infallible! What is your name? ",  {get_username, #user{}}}.

handle_data(Data, {confirm_password, User = #user{password = Password}}) ->
    MD5 = utils:md5(Data),
    error_logger:info_msg("[~p][~p:handle_data] Comparing hashes: ~p / ~p~n", [self(), ?MODULE, MD5, Password]),
    if
        MD5 == Password ->
            [
                {send_message, "Very good. Now, tell me about yourself...", [{echo, on}], complete},
                {upgrade, create_character, [{user, User}]}
            ];
        true ->
            {send_message, "\r\nPasswords do not match. Please enter your password: ", {choose_password, User}}
    end;
handle_data(Data, {choose_password, User}) ->
    MD5 = utils:md5(Data),
    {send_message, "\r\nPlease confirm your password: ", [{echo, off}], {
            confirm_password, 
            User#user{password = MD5}
        }};
handle_data(Data, {confirm_username, State}) ->
    Y = utils:re_match(Data, "ye?s?", [caseless]),
    N = utils:re_match(Data, "no?", [caseless]),
    if
        Y       -> {send_message, "Choose a password: ", [{echo, off}], {choose_password, State}};
        N       -> {send_message, "What is your name? ", {get_username, State}};
        true    -> {send_message, "Huh? ", State}
    end;
handle_data(Data, {get_username, _}) ->
    case infallible_users:get_user(Data) of
        undefined -> 
            {send_message, "Is this the name you wish to choose? [y/n]", {
                    confirm_username,
                    #user{username = Data}
                }};
        User when is_record(User, user) -> 
            {send_message, "What is your password?", [{echo, off}], {
                    get_password,
                    User
                }}
    end.


handle_info(Data, State) ->
    error_logger:info_msg("[~p][~p:handle_info] Handling info: ~p~n", [self(), ?MODULE, Data]),
    {ok, State}.

terminate(Reason, _State) ->
    error_logger:info_msg("[~p][~p:terminate] Terminating for reason: ~p~n", [self(), ?MODULE, Reason]),
    ok.
