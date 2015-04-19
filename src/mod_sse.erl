%%%---------------------------------------------------------------------------
%%% @doc
%%%   `inets'/`httpd' request handler module.
%%%
%%%   == httpd configuration ==
%%%
%%%   Of course you need to include `mod_sse' in `modules' section in `httpd'
%%%   config. With this done, you specify 3-tuples with URIs that are to be
%%%   handled by `mod_sse':
%%%
%%%   ```
%%%     HTTPDConfig = [
%%%       % ...
%%%       {modules, [mod_sse, ...]},
%%%       % ...
%%%       {sse, "/events", sse_handler},
%%%       % ...
%%%     ],
%%%   '''
%%%
%%%   Such tuple has `sse' atom as the first element, then URI under which
%%%   `mod_sse' operates, and a {@link gen_sse_server. callback module} that
%%%   produces (or, most probably, receives) messages to be sent to connected
%%%   clients.
%%%
%%%   The callback module is specified as either
%%%   {@type @{module(), Args :: [term()]@}} or
%%%   {@type module()} (`Args' default to `[]' in such case).
%%%
%%% @see gen_sse_server
%%% @end
%%%---------------------------------------------------------------------------

-module(mod_sse).

%%% `httpd' module API
-export([do/1]).

%%% enable qlc parse transform
-include_lib("stdlib/include/qlc.hrl").

%%%---------------------------------------------------------------------------

%%% httpd's data record
-include_lib("inets/include/httpd.hrl").

-define(CONTENT_TYPE, "text/event-stream").

%%%---------------------------------------------------------------------------

%% @private
%% @doc `httpd' handler.

do(_ModData = #mod{config_db = ConfigTable, request_uri = URI,
                   data = RequestData, socket = Socket}) ->
  case find_prefix(ConfigTable, URI) of
    nothing ->
      % nothing found, pass the request over
      {proceed, RequestData};
    {ok, {RootURI, Handler}} ->
      Code = 200, % HTTP OK
      Headers = [{content_type, ?CONTENT_TYPE}],
      Function = fun pass_received_events/5,
      ReqHeaders = [],
      Args = [Socket, Handler, RootURI, URI, ReqHeaders],
      Body = {Function, Args},
      {break, [{response, {response, [{code, Code} | Headers], Body}}]}
  end.

%%%---------------------------------------------------------------------------

-spec pass_received_events(gen_tcp:socket(), module(),
                           string(), string(), [{term(), term()}]) ->
  sent.

pass_received_events(Socket, Handler, RootURI, URI, ReqHeaders) ->
  % TODO: SSL sockets
  case Handler of
    Mod when is_atom(Mod) -> ModArgs = [];
    {Mod, ModArgs} when is_atom(Mod), is_list(ModArgs) -> ok
  end,
  {ok, RecPid} = mod_sse_sup:start_worker(Mod, ModArgs, self(),
                                          RootURI, URI, ReqHeaders),
  Ref = erlang:monitor(process, RecPid),
  inet:setopts(Socket, [{active, true}]),
  receive_and_pass(Socket, {Ref, RecPid}),
  % tell the httpd that all the response was already sent
  sent.

receive_and_pass(Socket, {Ref, Pid} = Receiver) ->
  receive
    {event, Data} ->
      Lines = binary:split(iolist_to_binary(Data), <<"\n">>),
      gen_tcp:send(Socket, [[["data: ", L, "\r\n"] || L <- Lines], "\r\n"]),
      receive_and_pass(Socket, Receiver);
    %{event, Id, Data}
    %{event, Id, EventType, Data}
    {'DOWN', Ref, process, Pid, _Info} ->
      ok;
    {tcp_closed, Socket} ->
      erlang:demonitor(Ref, [flush]),
      Pid ! {tcp_closed, self()},
      ok;
    {tcp_error, Socket, _Reason} ->
      erlang:demonitor(Ref, [flush]),
      Pid ! {tcp_closed, self()},
      ok;
    _Any ->
      % ignore
      % FIXME: how about system messages?
      receive_and_pass(Socket, Receiver)
  end.

%%%---------------------------------------------------------------------------

-spec find_prefix(ets:tab(), string()) ->
  {ok, {string(), module()}}.

find_prefix(ConfigTable, URI) ->
  Q = qlc:q([
    {Root, Handler} ||
    {sse, Root, Handler} <- ets:table(ConfigTable),
    is_uri_prefix_of(Root, URI)
  ]),
  % thanks to the sorting (DESC), nested prefixes should work consistently
  case qlc:e(qlc:keysort(1, Q, {order, descending})) of
    [] -> nothing;
    [{Root, Handler} | _] -> {ok, {Root, Handler}}
  end.

%%%---------------------------------------------------------------------------

is_uri_prefix_of("" = _Pfx, "" = _String) ->
  true;
is_uri_prefix_of("" = _Pfx, "/" ++ _String) ->
  true;
is_uri_prefix_of("" = _Pfx, "?" ++ _String) ->
  true;
is_uri_prefix_of("/" = _Pfx, "/" ++ _String) ->
  true;
is_uri_prefix_of([C | Pfx], [C | String]) ->
  is_uri_prefix_of(Pfx, String);
is_uri_prefix_of(_Pfx, _String) ->
  false.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
