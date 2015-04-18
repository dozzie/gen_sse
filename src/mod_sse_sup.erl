%%%---------------------------------------------------------------------------
%%% @doc
%%%   Supervisor for workers that receive events and pass them to appropriate
%%%   httpd process.
%%% @end
%%%---------------------------------------------------------------------------

-module(mod_sse_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_worker/6]).
-export([init/1]).

%%%---------------------------------------------------------------------------

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_worker(module(), [term()], pid(),
                   string(), string(), [{term(), term()}]) ->
  {ok, pid()} | {error, term()}.

start_worker(HandlerModule, HandlerArgs, ClientPid, RootURI, URI, ReqHeaders) ->
  Args = [HandlerModule, HandlerArgs, ClientPid, RootURI, URI, ReqHeaders],
  supervisor:start_child(?MODULE, Args).

%%%---------------------------------------------------------------------------

init([] = _Args) ->
  Strategy = {simple_one_for_one, 5, 10},
  Children = [
    {undefined,
      {mod_sse_worker, start_link, []},
      permanent, 5000, worker, [mod_sse_worker]}
  ],
  {ok, {Strategy, Children}}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
