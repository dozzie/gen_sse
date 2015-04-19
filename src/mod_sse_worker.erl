%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Process that receives events and passes them to httpd connection
%%%   handler.
%%% @end
%%%---------------------------------------------------------------------------

-module(mod_sse_worker).

-behaviour(gen_server).

-export([start_link/6]).

%%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------

-record(state, {
  mod :: module(),
  state :: term(),
  httpd :: pid()
}).

%%%---------------------------------------------------------------------------

%% @doc Start the receiver process.

-spec start_link(module(), [term()], pid(),
                 string(), string(), [{string(), string()}]) ->
    {ok, pid()} | ignore | {error, term()}.

start_link(Handler, HArgs, HTTPD, RootURI, URI, Headers) ->
  Args = [Handler, HArgs, HTTPD, RootURI, URI, Headers],
  gen_server:start_link(?MODULE, Args, []).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% starting and termination {{{

%% @private
%% @doc Initialize {@link gen_server} state.

init([Handler, HArgs, HTTPD, RootURI, URI, Headers] = _Args) ->
  % TODO: monitor HTTPD
  case Handler:init(RootURI, URI, Headers, HArgs) of
    {ok, HState} ->
      State = #state{mod = Handler, state = HState, httpd = HTTPD},
      {ok, State};
    {ok, HState, infinity = _Timeout} ->
      State = #state{mod = Handler, state = HState, httpd = HTTPD},
      {ok, State};
    {ok, HState, Timeout} when is_integer(Timeout) ->
      State = #state{mod = Handler, state = HState, httpd = HTTPD},
      {ok, State, Timeout};
    {ok, HState, hibernate} ->
      State = #state{mod = Handler, state = HState, httpd = HTTPD},
      {ok, State, hibernate};
    {stop, Reason} ->
      {stop, Reason}
  end.

%% @private
%% @doc Clean up {@link gen_server} state.

terminate(Reason, _State = #state{mod = Handler, state = HState}) ->
  Handler:terminate(Reason, HState).

%% }}}
%%----------------------------------------------------------
%% communication {{{

%% @private
%% @doc Handle {@link gen_server:call/2}.

handle_call(Request, From, State = #state{mod = Handler, state = HState}) ->
  case Handler:handle_call(Request, From, HState) of
    {reply, Reply, Events, NewHState} ->
      send_events_if_any(Events, State),
      NewState = State#state{state = NewHState},
      {reply, Reply, NewState};
    {reply, Reply, Events, NewHState, infinity = _Timeout} ->
      send_events_if_any(Events, State),
      NewState = State#state{state = NewHState},
      {reply, Reply, NewState, infinity};
    {reply, Reply, Events, NewHState, Timeout} when is_integer(Timeout) ->
      send_events_if_any(Events, State),
      NewState = State#state{state = NewHState},
      {reply, Reply, NewState, Timeout};
    {reply, Reply, Events, NewHState, hibernate} ->
      send_events_if_any(Events, State),
      NewState = State#state{state = NewHState},
      {reply, Reply, NewState, hibernate};
    {noreply, Events, NewHState} ->
      send_events_if_any(Events, State),
      NewState = State#state{state = NewHState},
      {noreply, NewState};
    {noreply, Events, NewHState, infinity = _Timeout} ->
      send_events_if_any(Events, State),
      NewState = State#state{state = NewHState},
      {noreply, NewState, infinity};
    {noreply, Events, NewHState, Timeout} when is_integer(Timeout) ->
      send_events_if_any(Events, State),
      NewState = State#state{state = NewHState},
      {noreply, NewState, Timeout};
    {noreply, Events, NewHState, hibernate} ->
      send_events_if_any(Events, State),
      NewState = State#state{state = NewHState},
      {noreply, NewState, hibernate};
    {stop, Reason, Reply, Events, NewHState} ->
      send_events_if_any(Events, State),
      NewState = State#state{state = NewHState},
      {stop, Reason, Reply, NewState};
    {stop, Reason, Events, NewHState} ->
      send_events_if_any(Events, State),
      NewState = State#state{state = NewHState},
      {stop, Reason, NewState}
  end.

%% @private
%% @doc Handle {@link gen_server:cast/2}.

handle_cast(Request, State = #state{mod = Handler, state = HState}) ->
  case Handler:handle_cast(Request, HState) of
    {noreply, Events, NewHState} ->
      send_events_if_any(Events, State),
      NewState = State#state{state = NewHState},
      {noreply, NewState};
    {noreply, Events, NewHState, infinity = _Timeout} ->
      send_events_if_any(Events, State),
      NewState = State#state{state = NewHState},
      {noreply, NewState, infinity};
    {noreply, Events, NewHState, Timeout} when is_integer(Timeout) ->
      send_events_if_any(Events, State),
      NewState = State#state{state = NewHState},
      {noreply, NewState, Timeout};
    {noreply, Events, NewHState, hibernate} ->
      send_events_if_any(Events, State),
      NewState = State#state{state = NewHState},
      {noreply, NewState, hibernate};
    {stop, Reason, Events, NewHState} ->
      send_events_if_any(Events, State),
      NewState = State#state{state = NewHState},
      {stop, Reason, NewState}
  end.

%% @private
%% @doc Handle incoming messages.

handle_info({tcp_closed, HTTPD} = _Message, State = #state{httpd = HTTPD}) ->
  {stop, normal, State};

handle_info(Message, State = #state{mod = Handler, state = HState}) ->
  case Handler:handle_info(Message, HState) of
    {noreply, Events, NewHState} ->
      send_events_if_any(Events, State),
      NewState = State#state{state = NewHState},
      {noreply, NewState};
    {noreply, Events, NewHState, infinity = _Timeout} ->
      send_events_if_any(Events, State),
      NewState = State#state{state = NewHState},
      {noreply, NewState, infinity};
    {noreply, Events, NewHState, Timeout} when is_integer(Timeout) ->
      send_events_if_any(Events, State),
      NewState = State#state{state = NewHState},
      {noreply, NewState, Timeout};
    {noreply, Events, NewHState, hibernate} ->
      send_events_if_any(Events, State),
      NewState = State#state{state = NewHState},
      {noreply, NewState, hibernate};
    {stop, Reason, Events, NewHState} ->
      send_events_if_any(Events, State),
      NewState = State#state{state = NewHState},
      {stop, Reason, NewState}
  end.

%% }}}
%%----------------------------------------------------------
%% code change {{{

%% @private
%% @doc Handle code change.

code_change(OldVsn, State = #state{mod = Handler, state = HState}, Extra) ->
  case Handler:code_change(OldVsn, HState, Extra) of
    {ok, NewHState} ->
      NewState = State#state{state = NewHState},
      {ok, NewState};
    {error, Reason} ->
      {error, Reason}
  end.

%% }}}
%%----------------------------------------------------------

-spec send_events_if_any([iolist()] | nothing, #state{}) ->
  ok.

send_events_if_any(nothing = _Events, _State) ->
  ok;

send_events_if_any(Events, _State = #state{httpd = HTTPD}) ->
  [HTTPD ! {event, E} || E <- Events],
  ok.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
