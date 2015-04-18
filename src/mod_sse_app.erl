%%%---------------------------------------------------------------------------
%%% @doc
%%%   `mod_sse' application entry point.
%%% @end
%%%---------------------------------------------------------------------------

-module(mod_sse_app).

-behaviour(application).

-export([start/2, stop/1]).

%%%---------------------------------------------------------------------------

start(_StartType, _StartArgs) ->
  mod_sse_sup:start_link().

stop(_State) ->
  ok.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
