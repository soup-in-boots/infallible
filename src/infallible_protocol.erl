-module(infallible_protocol).
-behaviour(cowboy_protocol).
-export([start_link/4,init/4]).
-include("infallible.hrl").
-define(use_callback(State,H,F,D,S), 
    case catch H:F(D,S) of
        {error, Reason}     -> 
            error_logger:info_msg("[~p][~p:?use_callback] Failure Notes: ~p", [self(), ?MODULE, [{state, State}, {handler, H}, {callback, F}, {input, D}, {handler_state, S}]]),
            die_error(Reason, State);
        {'EXIT', Reason}    -> 
            error_logger:info_msg("[~p][~p:?use_callback] Failure Notes: ~p", [self(), ?MODULE, [{state, State}, {handler, H}, {callback, F}, {input, D}, {handler_state, S}]]),
            die_error(Reason, State);
        HR                  -> 
            NewState = case HR of
                [_|_]   -> do_handler_batch(State, HR);
                _       -> do_handler(State, HR)
            end,
            main_loop(NewState)
    end).

-record(state, {
        listener,
        socket,
        transport,
        handler,
        handler_state
    }).

-define(IAC,        255).
-define(WILL,       251).
-define(WONT,       252).
-define(DO,         253).
-define(DONT,       254).
-define(ECHO,       1).
-define(EL,         248).
-define(LINE_MODE,  34).

start_link(ListenerPid, Socket, Transport, Opts) ->
    {ok, spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts])}.

init(ListenerPid, Socket, Transport, Opts) ->
    error_logger:info_msg("[~p][~p:init] Options: ~p~n", [self(), ?MODULE, Opts]),
    Handler         = infallible_utils:get_value(handler, Opts, login_handler),
    error_logger:info_msg("[~p][~p:init] Using Handler: ~p~n", [self(), ?MODULE, Handler]),
    HandlerOpts     = infallible_utils:get_value(handler_opts, Opts, []),
    HandlerState    = Handler:init(HandlerOpts),
    Transport:setopts(Socket, [{active, true}]),
    cowboy:accept_ack(ListenerPid),
    self() ! set_line_mode,
    main_loop(do_handler(#state{
            listener        = ListenerPid,
            socket          = Socket,
            transport       = Transport,
            handler         = Handler,
            handler_state   = HandlerState
        }, HandlerState)).

apply_options([], _, _)                     -> ok;
apply_options([{echo, off}|Options], S, T)  -> send_command(will_echo, S, T), apply_options(Options, S, T);
apply_options([{echo, on}|Options], S, T)   -> send_command(wont_echo, S, T), apply_options(Options, S, T);
apply_options([_|_], _, _)                  -> throw(badarg).

send_command(will_echo, S, T)   -> T:send(S, [?IAC, ?WILL, ?ECHO]);
send_command(wont_echo, S, T)   -> T:send(S, [?IAC, ?WONT, ?ECHO]);
send_command(clear_line, S, T)  -> T:send(S, [?IAC, ?EL]);
send_command(line_mode, S, T)   -> T:send(S, [?IAC, ?LINE_MODE]).

handle_command(C, S) ->
    error_logger:info_msg("[~p][~p:handle_command] Command: ~p~n", [self(), ?MODULE, C]),
    S.

main_loop(State = #state{socket = S, transport = T, handler = H, handler_state = HS}) ->
    receive
        {tcp, S, Data = <<?IAC,?IAC,_Other/binary>>} -> 
            error_logger:info_msg("[~p][~p:main_loop] Double IAC recognized: ~p~n", [self(), ?MODULE, Data]),
            ?use_callback(State, H, handle_data, re:replace(unicode:characters_to_list(Data), "\\s*$", "", [global, {return, list}]), HS);
        {tcp, S, Data = <<?IAC,_Other/binary>>} -> 
            main_loop(handle_command(Data, State));
        {tcp, S, Data} -> 
            ?use_callback(State, H, handle_data, re:replace(unicode:characters_to_list(Data), "\\s*$", "", [global, {return, list}]), HS);

        set_line_mode -> 
            send_command(line_mode, S, T), 
            main_loop(State);
        Anything -> 
            ?use_callback(State, H, handle_info, Anything, HS)
    end.

do_handler(State = #state{transport = T, socket = S}, {send_message, Response, Options, HandlerState}) ->
    apply_options(Options, S, T),
    do_handler(State, {send_message, Response, HandlerState});
do_handler(State = #state{transport = T, socket = S}, {send_message, Response, HandlerState}) ->
    T:send(S, Response),
    State#state{handler_state = HandlerState};
do_handler(State, {upgrade, NewHandler, NewHandlerOpts}) ->
    HS = NewHandler:init(NewHandlerOpts),
    State#state{handler = NewHandler, handler_state = HS};
do_handler(State, {ok, HandlerState}) ->
    State#state{handler_state = HandlerState}.

do_handler_batch(State, [])     -> State;
do_handler_batch(State, [C|T])  -> do_handler_batch(do_handler(State, C), T).

die_error(Reason, _State = #state{transport = T, socket = S, handler = H, handler_state = HS}) ->
    H:terminate(Reason, HS),
    T:close(S).
