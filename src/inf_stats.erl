-module(inf_stats).
-export([get_interface/1]).
-export([calculate_stats/1]).
-export([grab/2]).
-include("infallible.hrl").

get_interface(Stats = #stats{}) ->
    erlv8_object:new(lists:zip(record_info(fields, stats), tl(tuple_to_list(Stats)))).

grab(Stats = #stats{}, Field) ->
    Fields = record_info(fields, stats),
    Values = tl(tuple_to_list(Stats)),
    utils:grab_field(Field, Fields, Values).

calculate_stats(Entity = #entity{base = Base, race = _Race = #race{stats = RaceStats}}) ->
    Max1 = Base#stats{
        hps         = Base#stats.hps + RaceStats#stats.hps,
        mps         = Base#stats.mps + RaceStats#stats.mps,
        sps         = Base#stats.sps + RaceStats#stats.sps,

        str         = Base#stats.str + RaceStats#stats.str,
        dex         = Base#stats.dex + RaceStats#stats.dex,
        con         = Base#stats.con + RaceStats#stats.con,
        int         = Base#stats.int + RaceStats#stats.int,
        spr         = Base#stats.spr + RaceStats#stats.spr
    },
    Max2 = Max1#stats{
        health      = calculate_health(Max1),
        mana        = calculate_mana(Max1),
        stamina     = calculate_stamina(Max1),

        hps         = calculate_hps(Max1),
        mps         = calculate_mps(Max1),
        sps         = calculate_sps(Max1)
    },
    calculate_stats_2(Entity#entity{active_max = Max2}).

calculate_stats_2(Entity = #entity{active_max = Max, active = undefined}) ->
    Entity#entity{active = Max};
calculate_stats_2(Entity = #entity{active_max = Max, active = Active}) ->
    Entity#entity{active = Max#stats{
            health      = lists:min([Active#stats.health, Max#stats.health]),
            mana        = lists:min([Active#stats.mana, Max#stats.mana]),
            stamina     = lists:min([Active#stats.stamina, Max#stats.stamina])
        }}.

calculate_health(_ = #stats{str = Str, con = Con}) ->
    (2 * Str) + (6 * Con).

calculate_mana(_ = #stats{int = Int, spr = Spr}) ->
    (1 * Int) + (3 * Spr).

calculate_stamina(_ = #stats{dex = Dex, con = Con}) ->
    (1 * Dex) + (3 * Con).

calculate_hps(_ = #stats{hps = HPS, con = Con}) ->
    trunc(HPS + ((Con * 2) / 10)).

calculate_mps(_ = #stats{mps = MPS, spr = Spr}) ->
    trunc(MPS + ((Spr * 1) / 10)).

calculate_sps(_ = #stats{sps = SPS, con = Con}) ->
    trunc(SPS + ((Con * 1) / 10)).

