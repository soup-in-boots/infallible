-module(inf_protocol).
-behaviour(ranch_protocol).
-behaviour(gen_server).
-export([start_link/4,init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).
-export([send_message/2, send_message/3]).
-export([set_cursor/2]).
-include("infallible.hrl").
-define(use_callback(State,H,F,D,S), 
    case catch H:F(D,S,State) of
        {'EXIT', Reason}    -> 
            error_logger:info_msg("[~p][~p:?use_callback] Failure Notes: ~p", [self(), ?MODULE, [{state, State}, {handler, H}, {callback, F}, {input, D}, {handler_state, S}]]),
            die_error(Reason, State);
        HR                  -> 
            error_logger:info_msg("[~p][~p:?use_callback] Handler Response: ~p", [self(), ?MODULE, HR]),
            {noreply, do_handler(HR)}
    end).

-record(state, {
        listener,
        socket,
        transport,
        handler,
        handler_state,
        cursor
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
    gen_server:start_link(?MODULE, [ListenerPid, Socket, Transport, Opts], []).

init([ListenerPid, Socket, Transport, Opts]) ->
    error_logger:error_msg("INF Protocol Handler Initiated"),
    process_flag(trap_exit, true),
    Transport:setopts(Socket, [{active, true}]),
    Handler         = utils:get_value(handler, Opts, login_handler),
    HandlerOpts     = utils:get_value(handler_opts, Opts, []),
    State           = #state{
            listener        = ListenerPid,
            socket          = Socket,
            transport       = Transport,
            handler         = Handler,
            handler_state   = undefined
        },
    HandlerState    = Handler:init(HandlerOpts, State),
    self() ! accept_ack,
    self() ! set_line_mode,
    error_logger:error_msg("Initialized."),
    {ok, do_handler(HandlerState)}.

apply_options([], _, _)                     -> ok;
apply_options([{echo, off}|Options], S, T)  -> send_command(will_echo, S, T), apply_options(Options, S, T);
apply_options([{echo, on}|Options], S, T)   -> send_command(wont_echo, S, T), apply_options(Options, S, T);
apply_options([_|_], _, _)                  -> throw(badarg).

send_command(will_echo, S, T)   -> T:send(S, [?IAC, ?WILL, ?ECHO]);
send_command(wont_echo, S, T)   -> T:send(S, [?IAC, ?WONT, ?ECHO]);
send_command(clear_line, S, T)  -> T:send(S, [?IAC, ?EL]);
send_command(line_mode, S, T)   -> T:send(S, [?IAC, ?LINE_MODE]).

set_cursor(State, Cursor) ->
    State#state{cursor = Cursor}.

handle_command(C, S) ->
    error_logger:info_msg("[~p][~p:handle_command] Command: ~p~n", [self(), ?MODULE, C]),
    S.

handle_call(Call, From, State = #state{handler = H, handler_state = HS}) ->
    case catch H:handle_call(Call, From, HS, State) of
        {'EXIT', Reason} -> 
            die_error(Reason, State);
        {reply, Reply, NewHS, State1} -> 
            State2 = do_handler({ok, NewHS, State1}),
            {reply, Reply, State2}
    end.

handle_cast(Cast, State = #state{handler = H, handler_state = HS}) ->
    ?use_callback(State, H, handle_cast, Cast, HS).

handle_info({tcp, S, Data = <<?IAC,?IAC,_Other/binary>>}, State = #state{socket = S, handler = H, handler_state = HS}) ->
    error_logger:info_msg("[~p][~p:main_loop] Double IAC recognized: ~p~n", [self(), ?MODULE, Data]),
    ?use_callback(State, H, handle_data, re:replace(unicode:characters_to_list(Data), "\\s*$", "", [global, {return, list}]), HS);
handle_info({tcp, S, Data = <<?IAC,_Other/binary>>}, State = #state{socket = S}) ->
    {noreply, handle_command(Data, State)};
handle_info({tcp, S, Data}, State = #state{socket = S, handler = H, handler_state = HS}) ->
    ?use_callback(State, H, handle_data, re:replace(unicode:characters_to_list(Data), "\\s*$", "", [global, {return, list}]), HS);
handle_info(set_line_mode, State = #state{socket = S, transport = T}) ->
    send_command(line_mode, S, T), 
    {noreply, State};
handle_info(accept_ack, State = #state{listener = ListenerPid}) ->
    ranch:accept_ack(ListenerPid),
    {noreply, State};
handle_info(Anything, State = #state{handler = H, handler_state = HS}) ->
    ?use_callback(State, H, handle_info, Anything, HS).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, State) ->
    die_error(Reason, State).

send_message(State = #state{transport = T, socket = S, handler_state = HS}, Message, Options) ->
    apply_options(Options, S, T),
    send_message(State, Message).
    
send_message(State = #state{cursor = Cursor, transport = T, socket = S, handler_state = HS}, Message) ->
    T:send(S, Message),
    case Cursor of
        undefined ->
            ok;
        F when is_function(F, 2) ->
            C = F(HS, State),
            T:send(S, C)
    end.

do_handler({upgrade, NewHandler, NewHandlerOpts, State = #state{handler = Handler, handler_state = HandlerState}}) ->
    Handler:terminate(upgrade, HandlerState),
    NewState = State#state{handler = NewHandler},
    Res = NewHandler:init(NewHandlerOpts, NewState),
    error_logger:error_msg("Upgrade init complete. Result: ~p", [Res]),
    do_handler(Res);
do_handler({ok, HandlerState, NewState}) ->
    NewState#state{handler_state = HandlerState};
do_handler({stop, Reason, State}) ->
    die_error({stop, Reason}, State).

die_error(Reason, _State = #state{transport = T, socket = S, handler = H, handler_state = HS}) ->
    error_logger:error_msg("INF Protocol Handler Terminating: ~p", [Reason]),
    H:terminate(Reason, HS),
    T:close(S),
    {stop, Reason}.
