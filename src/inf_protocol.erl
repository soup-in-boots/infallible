-module(inf_protocol).
-behaviour(ranch_protocol).
-behaviour(gen_server).
-export([start_link/4,init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).
-export([send_message/2, send_message/3]).
-export([set_cursor/2]).
-export([fg_color/1, bg_color/1, reset_code/0]).
-include("infallible.hrl").
-define(DUMP_INTERVAL, 50).
-define(use_callback(State,H,F,D,S), 
    case catch H:F(D,S,State) of
        {'EXIT', Reason}    -> 
            die_error(Reason, State);
        HR -> 
            do_handler(HR)
    end).

-record(state, {
        listener,
        socket,
        transport,
        handler,
        handler_state,
        cursor,
        cursor_since_recv = false,
        message_queue = []
    }).

-define(IAC,        255).
-define(WILL,       251).
-define(WONT,       252).
-define(DO,         253).
-define(DONT,       254).
-define(ECHO,       1).
-define(EL,         248).
-define(LINE_MODE,  34).
-define(ANSI(C),    <<16#1b,$[,C,$m>>).

fg_color(black) -> ?ANSI("30");
fg_color(red) -> ?ANSI("31");
fg_color(green) -> ?ANSI("32");
fg_color(yellow) -> ?ANSI("33");
fg_color(blue) -> ?ANSI("34");
fg_color(purple) -> ?ANSI("36");
fg_color(magenta) -> ?ANSI("36");
fg_color(cyan) -> ?ANSI("37");
fg_color(white) -> ?ANSI("38");
fg_color(default) -> ?ANSI("39").

bg_color(black) -> ?ANSI("40");
bg_color(red) -> ?ANSI("41");
bg_color(green) -> ?ANSI("42");
bg_color(yellow) -> ?ANSI("43");
bg_color(blue) -> ?ANSI("44");
bg_color(purple) -> ?ANSI("46");
bg_color(magenta) -> ?ANSI("46");
bg_color(cyan) -> ?ANSI("47");
bg_color(white) -> ?ANSI("48");
bg_color(default) -> ?ANSI("49").

reset_code() -> ?ANSI("0").

start_link(ListenerPid, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [ListenerPid, Socket, Transport, Opts], []).

init([ListenerPid, Socket, Transport, Opts]) ->
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
    HandlerResult = Handler:init(HandlerOpts, State),
    self() ! accept_ack,
    self() ! set_line_mode,
    case do_handler(HandlerResult) of
        {noreply, State2, DI} -> {ok, State2, DI};
        Result -> Result
    end.

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
    S.

handle_call(Call, From, State = #state{handler = H, handler_state = HS}) ->
    case catch H:handle_call(Call, From, HS, State) of
        {'EXIT', Reason} -> 
            die_error(Reason, State);
        {reply, Reply, NewHS, State2} -> 
            State3 = do_handler({ok, NewHS, State2}),
            {reply, Reply, State3, dump_interval(State3)};
        Res ->
            do_handler(Res)
    end.

handle_cast(Cast, State = #state{handler = H, handler_state = HS}) ->
    ?use_callback(State, H, handle_cast, Cast, HS).

handle_info(timeout, State) ->
    io:format("timeout :: flush~n"),
    {noreply, do_dump(State)};
handle_info({tcp_closed, _S}, State) ->
    die_error(connection_lost, State);
handle_info({tcp, S, Data = <<?IAC,?IAC,_Other/binary>>}, State = #state{socket = S, handler = H, handler_state = HS}) ->
    ?use_callback(State, H, handle_data, re:replace(Data, "\\s*$", "", [global, {return, binary}]), HS);
handle_info({tcp, S, Data = <<?IAC,_Other/binary>>}, State = #state{socket = S}) ->
    State2 = handle_command(Data, State),
    {noreply, State2, dump_interval(State2)};
handle_info({tcp, S, Data}, State = #state{transport = T, socket = S, handler = H, handler_state = HS}) ->
    ?use_callback(State#state{cursor_since_recv = false}, H, handle_data, re:replace(Data, "\\s*$", "", [global, {return, binary}]), HS);
handle_info(set_line_mode, State = #state{socket = S, transport = T}) ->
    send_command(line_mode, S, T), 
    {noreply, State, dump_interval(State)};
handle_info(accept_ack, State = #state{listener = ListenerPid}) ->
    ranch:accept_ack(ListenerPid),
    {noreply, State, dump_interval(State)};
handle_info(Anything, State = #state{handler = H, handler_state = HS}) ->
    ?use_callback(State, H, handle_info, Anything, HS).

code_change(_OldVsn, State, _Extra) ->
    {ok, State, dump_interval(State)}.

terminate(Reason, State) ->
    die_error(Reason, State).

send_message(State, Message) ->
    send_message(State, Message, []).

send_message(State = #state{transport = T, socket = S, handler_state = HS}, Message, Options) ->
    Force = proplists:get_value(force, Options, false),
    NewOptions = proplists:delete(force, Options),
    apply_options(NewOptions, S, T),
    do_send_message(State, Message, Force).

do_send_message(State = #state{message_queue = MQ}, Message, true) ->
    do_dump(State#state{message_queue = [MQ|Message]});
do_send_message(State = #state{message_queue = MQ}, Message, false) ->
    State#state{message_queue = [MQ|Message]}.

do_handler({upgrade, NewHandler, NewHandlerOpts, State = #state{handler = Handler, handler_state = HandlerState}}) ->
    Handler:terminate(upgrade, HandlerState),
    NewState = State#state{handler = NewHandler},
    Res = NewHandler:init(NewHandlerOpts, NewState),
    do_handler(Res);
do_handler({ok, HandlerState, State}) ->
    State2 = State#state{
        handler_state = HandlerState
    },
    DI = dump_interval(State2),
    {noreply, State2, DI};
do_handler({stop, Reason, HS, State}) ->
    State2 = do_dump(State#state{
            handler_state = HS,
            cursor=undefined
        }),
    {stop, Reason, State2}.

die_error(Reason, State = #state{transport = T, socket = S, handler = H, handler_state = HS}) ->
    H:terminate(Reason, HS),
    T:close(S),
    {stop, Reason, State}.

do_dump(State = #state{cursor = undefined}) -> 
    do_dump_2(State);
do_dump(State = #state{handler_state = HS, cursor = Cursor, cursor_since_recv = false, message_queue = MQ}) when is_function(Cursor, 2) ->
    do_dump_2(State#state{cursor_since_recv = true, message_queue = [MQ|Cursor(HS, State)]});
do_dump(State) ->
    do_dump_2(State).

do_dump_2(State = #state{message_queue = []}) ->
    State;
do_dump_2(State = #state{transport = T, socket = S, message_queue = MQ}) ->
    T:send(S, MQ),
    State#state{message_queue = []}.

dump_interval(_ = #state{message_queue = []}) -> infinity;
dump_interval(_) -> ?DUMP_INTERVAL.
