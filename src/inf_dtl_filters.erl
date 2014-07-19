-module(inf_dtl_filters).
-export([to_json/1]).

to_json(Object) ->
    jiffy:encode(Object).
