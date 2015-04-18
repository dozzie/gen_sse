%%%---------------------------------------------------------------------------
%%% @doc
%%%   Example config and startup code for inets.
%%%
%%%   == starting inets ==
%%%
%%%   ```
%%%   start_httpd() ->
%%%     % remember to start `inets' application
%%%     {ok, Config} = load_config(),
%%%     {ok, HttpdPid} = inets:start(httpd, Config),
%%%     {ok, HttpdPid}.
%%%
%%%   load_config() ->
%%%     SSEHandler = sse_handler,
%%%     DocumentRoot = "/var/www",
%%%     ServerRoot = "/var/lib/www",
%%%     Config = [
%%%       % typical setup for inets/httpd
%%%       {port, 1080}, {server_name, "localhost"},
%%%       {document_root, DocumentRoot}, {server_root, ServerRoot},
%%%       {directory_index, ["index.html"]}, % requires mod_alias
%%%
%%%       % httpd needs to load `mod_sse'
%%%       {modules, [mod_sse, mod_alias, mod_dir, mod_get, mod_log]},
%%%
%%%       % mod_sse's config
%%%       {sse, "/events", SSEHandler},
%%%
%%%       % logging
%%%       {transfer_log, "/var/log/httpd/access.log"},
%%%       {error_log,    "/var/log/httpd/error.log"},
%%%       {log_format, combined},
%%%       {error_log_format, pretty}
%%%     ],
%%%     {ok, Config}.
%%%   '''
%%%
%%%   == example handler module ==
%%%
%%%   Note: there's no `start_link()' function. You don't need it, as the
%%%   process will be spawned by {@link mod_sse} as necessary.
%%%
%%%   ```
%%%   -module(sse_handler).
%%%
%%%   -behaviour(gen_sse_server).
%%%
%%%   -export([init/4, terminate/2]).
%%%   -export([handle_call/3, handle_cast/2, handle_info/2]).
%%%   -export([code_change/3]).
%%%
%%%   -record(state, {}).
%%%
%%%   init(_RootURI, _URI, _Headers, _Args) ->
%%%     State = #state{},
%%%     {ok, State}.
%%%
%%%   terminate(_Reason, _State) ->
%%%     ok.
%%%
%%%   handle_call(_Request, _From, State) ->
%%%     Events = nothing,
%%%     {reply, {error,not_implemented}, Events, State}.
%%%
%%%   handle_cast(_Request, State) ->
%%%     Events = nothing,
%%%     {noreply, Events, State}.
%%%
%%%   handle_info(_Message, State) ->
%%%     Events = nothing,
%%%     {noreply, Events, State}.
%%%
%%%   code_change(OldVsn, State, Extra) ->
%%%     {ok, State}.
%%%   '''
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(example_inets).
