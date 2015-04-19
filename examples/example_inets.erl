%%%---------------------------------------------------------------------------
%%% @doc
%%%   Full example of running {@link mod_sse}.
%%%   The example includes how to start inets, simple event
%%%   receiver/forwarder, and receiver/forwarder registry, being
%%%   a distribution point for messages (in fan-out mode).
%%%
%%%   == starting inets ==
%%%
%```
%start_httpd() ->
%  % remember to start `inets' and `mod_sse' applications
%  {ok, Config} = load_config(),
%  {ok, HttpdPid} = inets:start(httpd, Config),
%  {ok, HttpdPid}.
%
%load_config() ->
%  SSEHandler = sse_handler,
%  DocumentRoot = "/var/www",
%  ServerRoot = "/var/lib/www",
%  Config = [
%    % typical setup for inets/httpd
%    {port, 1080}, {server_name, "localhost"},
%    {document_root, DocumentRoot}, {server_root, ServerRoot},
%    {directory_index, ["index.html"]}, % requires mod_alias
%
%    % httpd needs to load `mod_sse'
%    {modules, [mod_sse, mod_alias, mod_dir, mod_get, mod_log]},
%
%    % mod_sse's config
%    {sse, "/events", SSEHandler},
%
%    % logging
%    {transfer_log, "/var/log/httpd/access.log"},
%    {error_log,    "/var/log/httpd/error.log"},
%    {log_format, combined},
%    {error_log_format, pretty}
%  ],
%  {ok, Config}.
%'''
%%%
%%%   == example forwarder module ==
%%%
%%%   This module in its initialization phase asks registry to send
%%%   messages as they come.
%%%
%%%   Processes running this module will be spawned (by {@link mod_sse}) in
%%%   bulk under a `simple_one_for_one' supervisor.
%%%
%%%   <b>Note</b>: there's no need for `start_link()' function here.
%%%
%```
%-module(sse_handler).
%
%-behaviour(gen_sse_server).
%
%-export([init/4, terminate/2]).
%-export([handle_call/3, handle_cast/2, handle_info/2]).
%-export([code_change/3]).
%
%-record(state, {}).
%
%init(_RootURI, _URI, _Headers, _Args) ->
%  % XXX: see the registry module
%  sse_registry:subscribe(self()),
%  State = #state{},
%  {ok, State}.
%
%terminate(_Reason, _State) ->
%  ok.
%
%
%handle_call(_Request, _From, State) ->
%  Events = nothing,
%  {reply, {error,not_implemented}, Events, State}.
%
%
%handle_cast(_Request, State) ->
%  Events = nothing,
%  {noreply, Events, State}.
%
%
%handle_info({event, Event} = _Message, State) ->
%  Events = [Event], % XXX: this is what comes from registry module
%  {noreply, Events, State};
%handle_info(_Message, State) ->
%  Events = nothing,
%  {noreply, Events, State}.
%
%
%code_change(_OldVsn, State, _Extra) ->
%  {ok, State}.
%'''
%%%
%%%   == example registry module ==
%%%
%%%   Registry process receives subscription requests from `sse_handler' and
%%%   then forwards incoming messages to all subscribed processes. The
%%%   messages should be iolists and are sent with `sse_registry:send/1'
%%%   function.
%%%
%%%   Of course this process needs to be started in order for `sse_handler' to
%%%   work.
%%%
%```
%-module(sse_registry).
%
%-behaviour(gen_server).
%
%-export([start/0, start_link/0]).
%
%-export([subscribe/1]).
%-export([send/1]).
%
%-export([init/1, terminate/2]).
%-export([handle_call/3, handle_cast/2, handle_info/2]).
%-export([code_change/3]).
%
%-record(state, {}).
%
%start() ->
%  gen_server:start({local, ?MODULE}, ?MODULE, [], []).
%
%start_link() ->
%  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%
%
%send(Event) ->
%  gen_server:cast(?MODULE, {send, Event}).
%
%subscribe(Pid) ->
%  gen_server:call(?MODULE, {subscribe, Pid}).
%
%
%init([] = _Args) ->
%  ets:new(sse_subscriptions, [set, named_table]),
%  State = #state{},
%  {ok, State}.
%
%terminate(_Reason, _State) ->
%  ets:delete(sse_subscriptions),
%  ok.
%
%
%handle_call({subscribe, Pid} = _Request, _From, State) ->
%  Ref = erlang:monitor(process, Pid),
%  ets:insert(sse_subscriptions, {Pid, Ref}),
%  {reply, ok, State};
%handle_call(_Request, _From, State) ->
%  {reply, {error,not_implemented}, State}.
%
%
%handle_cast({send, Event} = _Request, State) ->
%  case unicode:characters_to_binary(Event, unicode) of
%    EventBin when is_binary(EventBin) ->
%      % fan-out distribution
%      ets:foldl(
%        fun({Pid, _Ref}, Acc) -> Pid ! {event, EventBin}, Acc end,
%        ok, sse_subscriptions
%      );
%    {error, _Bin, _Rest} ->
%      ignore;
%    {incomplete, _Bin, _Rest} ->
%      ignore
%  end,
%  {noreply, State};
%handle_cast(_Request, State) ->
%  {noreply, State}.
%
%
%handle_info({'DOWN', _Ref, process, Pid, _Info} = _Message, State) ->
%  ets:delete(sse_subscriptions, Pid),
%  {noreply, State};
%handle_info(_Message, State) ->
%  {noreply, State}.
%
%
%code_change(_OldVsn, State, _Extra) ->
%  {ok, State}.
%'''
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(example_inets).
