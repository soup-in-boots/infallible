-module(client_handler).
-export([init/1, handle_data/2, handle_info/2, terminate/2]).
-record(state, {
        username    = undefined,
        password    = undefined
    }).

init(_) ->
    error_logger:info_msg("[~p][~p:init] Woot!~n", [self(), ?MODULE]),
    {ok, #state{}}.

handle_data(Data, State) ->
    error_logger:info_msg("[~p][~p:handle_data] Handling Data: ~p~n", [self(), ?MODULE, Data]),
    {ok, State}.

handle_info(Data, State) ->
    error_logger:info_msg("[~p][~p:handle_info] Handling Info: ~p~n", [self(), ?MODULE, Data]),
    {ok, State}.

terminate(Reason, State) ->
    error_logger:info_msg("[~p][~p:terminate] Terminating Because: ~p~n", [self(), ?MODULE, Reason]),
    ok.
