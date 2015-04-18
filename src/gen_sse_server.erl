%%%---------------------------------------------------------------------------
%%% @doc
%%%   Behaviour for a callback module to use with {@link mod_sse}.
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_sse_server).

%%%---------------------------------------------------------------------------

-type event() :: iolist().

%%%---------------------------------------------------------------------------

-callback init(RootURI :: string(), URI :: string(),
               Headers :: [{term(), term()}], Args :: term()) ->
    {ok, term()}
  | {ok, term(), timeout()}
  | {ok, term(), hibernate}
  | {error, term()}.

-callback terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                    State :: term()) ->
  term().

-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), nothing | [event()], NewState :: term()}
  | {reply, Reply :: term(), nothing | [event()], NewState :: term(), timeout()}
  | {reply, Reply :: term(), nothing | [event()], NewState :: term(), hibernate}
  | {noreply, nothing | [event()], NewState :: term()}
  | {noreply, nothing | [event()], NewState :: term(), timeout()}
  | {noreply, nothing | [event()], NewState :: term(), hibernate}
  | {stop, Reason :: term(), Reply :: term(), nothing | [event()], NewState :: term()}
  | {stop, Reason :: term(), nothing | [event()], NewState :: term()}.

-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, nothing | [event()], NewState :: term()}
  | {noreply, nothing | [event()], NewState :: term(), timeout()}
  | {noreply, nothing | [event()], NewState :: term(), hibernate}
  | {stop, Reason :: term(), nothing | [event()], NewState :: term()}.

-callback handle_info(Message :: term(), State :: term()) ->
    {noreply, nothing | [event()], NewState :: term()}
  | {noreply, nothing | [event()], NewState :: term(), timeout()}
  | {noreply, nothing | [event()], NewState :: term(), hibernate}
  | {stop, Reason :: term(), nothing | [event()], NewState :: term()}.

-callback code_change(OldVsn :: term() | {down, term()}, State :: term(),
                      Extra :: term()) ->
    {ok, NewState :: term()}
  | {error, term()}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker:nowrap
