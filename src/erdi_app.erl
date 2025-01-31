%%%-------------------------------------------------------------------
%% @doc erdi public API
%% @end
%%%-------------------------------------------------------------------

-module(erdi_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erdi_sup:start_link().

stop(_State) ->
    ok.
