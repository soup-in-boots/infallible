-module(login_handler).
-export([init/2, handle_data/3, handle_info/3, terminate/2]).
-include("infallible.hrl").
    
init(_, Client) ->
    inf_protocol:send_message(Client, "Welcome to infallible! What is your name? "),
    {ok, {get_username, #user{}}, Client}.

handle_data(Data, {get_password, _User = #user{password = Password, entity = EntityID}}, Client) ->
    inf_protocol:send_message(Client, "\r\nWell, let's see here... Hmmm...\r\n"),
    timer:sleep(crypto:rand_uniform(1000, 3000)),
    inf_protocol:send_message(Client, "Aha! Here it is...\r\n"),
    timer:sleep(crypto:rand_uniform(1000, 3000)),
    MD5 = utils:md5(Data),
    case MD5 of
        Password ->
            inf_protocol:send_message(Client, "Yes... Yes... That's correct. Welcome back Darius.\r\n", [{echo, on}]),
            {upgrade, client_handler, [{entity, EntityID}], Client};
        _Other ->
            inf_protocol:send_message(Client, "Eh? That's not right at all! Get out of here, you scallywag!\r\n"),
            {stop, bad_password, Client}
    end;
handle_data(Data, {confirm_password, User = #user{password = Password}}, Client) ->
    MD5 = utils:md5(Data),
    error_logger:info_msg("[~p][~p:handle_data] Comparing hashes: ~p / ~p~n", [self(), ?MODULE, MD5, Password]),
    if
        MD5 == Password ->
            inf_protocol:send_message(Client, "Very good. Now, tell me about yourself...", [{echo, on}]),
            {upgrade, create_character, [{user, User}], Client};
        true ->
            inf_protocol:send_message(Client, "\r\nPasswords do not match. Please enter your password: "), 
            {ok, {choose_password, User}, Client}
    end;
handle_data(Data, {choose_password, User}, Client) ->
    MD5 = utils:md5(Data),
    inf_protocol:send_message(Client, "\r\nPlease confirm your password: ", [{echo, off}]), 
    {ok, {confirm_password, User#user{password = MD5}}, Client};
handle_data(Data, {confirm_username, State}, Client) ->
    Y = utils:re_match(Data, "ye?s?", [caseless]),
    N = utils:re_match(Data, "no?", [caseless]),
    if
        Y -> 
            inf_protocol:send_message(Client, "Choose a password: ", [{echo, off}]),
            {ok, {choose_password, State}, Client};
        N -> 
            inf_protocol:send_message(Client, "What is your name? "), 
            {ok, {get_username, State}, Client};
        true -> 
            inf_protocol:send_message(Client, "Huh? "),
            {ok, State, Client}
    end;
handle_data(Data, {get_username, _}, Client) ->
    case inf_user:fetch(Data) of
        undefined -> 
            inf_protocol:send_message(Client, "Is this the name you wish to choose? [y/n]"),
            {ok, {confirm_username, #user{username = Data}}, Client};
        User when is_record(User, user) -> 
            inf_protocol:send_message(Client, "What is your password? ", [{echo, off}]),
            {ok, {get_password, User}, Client}
    end.


handle_info(Data, State, Client) ->
    error_logger:info_msg("[~p][~p:handle_info] Handling info: ~p~n", [self(), ?MODULE, Data]),
    {ok, State, Client}.

terminate(Reason, _State) ->
    error_logger:info_msg("[~p][~p:terminate] Terminating for reason: ~p~n", [self(), ?MODULE, Reason]),
    ok.
