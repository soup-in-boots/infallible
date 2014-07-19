-module(inf_tick_manager).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-include("infallible.hrl").
-define(TICKERS, [inf_entity_manager]).
-define(SUPER_TICKERS, [inf_entity_manager, inf_room_manager]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    TickRef         = erlang:start_timer(?TICK, self(), tick),
    SuperTickRef    = erlang:start_timer(?SUPER_TICK, self(), super_tick),
    {ok, {TickRef, SuperTickRef}}.

handle_call(_Call, _From, State) ->
    {reply, undefined, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info({timeout, TickRef, tick}, {TickRef, SuperTickRef}) ->
    NewTickRef = erlang:start_timer(?TICK, self(), tick),
    lists:foreach(fun(Server) -> gen_server:cast(Server, tick) end, ?TICKERS),
    {noreply, {NewTickRef, SuperTickRef}};
handle_info({timeout, SuperTickRef, super_tick}, {TickRef, SuperTickRef}) ->
    NewSuperTickRef = erlang:start_timer(?SUPER_TICK, self(), super_tick),
    lists:foreach(fun(Server) -> gen_server:cast(Server, super_tick) end, ?SUPER_TICKERS),
    {noreply, {TickRef, NewSuperTickRef}};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

