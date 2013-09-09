%%%-----------------------------------------------------------------------------
%%% File       util2.erl
%%% @author    Gordon Guthrie <gordonguthrie@vixo.com>
%%% @doc
%%% @copyright Hypernumbers Ltd
%%% @private
%%%
%%% Created    22 Nov 2006 by Gordon Guthrie <gordonguthrie@vixo.com>
%%%-----------------------------------------------------------------------------
-module(util2).

-export([
         timestamp_to_date/1,
         get_timestamp/0
        ]).

-define(MEGA, 1000000000000).
-define(SEC,  1000000).

%%%-----------------------------------------------------------------------------
%%%
%%% Worker functions for the util2
%%% There are two util files cos you can't load one with try/catch into the
%%% debugger (ie that one)
%%%
%%%-----------------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These functions are all utility functions                                %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_timestamp() -> pos_integer().
get_timestamp()->
    {Mega, Sec, Micro} = now(),
    ?MEGA * Mega + ?SEC * Sec + Micro.

timestamp_to_date(Stamp) when is_list(Stamp) ->
    timestamp_to_date(list_to_integer(Stamp));
timestamp_to_date(Stamp) when is_integer(Stamp) ->
    Mega = trunc(Stamp/?MEGA),
    Sec = trunc((Stamp - Mega * ?MEGA)/?SEC),
    Micro = Stamp - Mega * ?MEGA - Sec * ?SEC,
    {Mega, Sec, Micro}.
