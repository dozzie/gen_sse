%%%---------------------------------------------------------------------------
%%% @doc
%%%   Behaviour for a callback module to use with {@link mod_sse}.
%%%
%%%   == Expected callbacks ==
%%%
%%%   The callbacks are very similar to the ones from {@link gen_server},
%%%   except that `handle_*()' return one more element in tuples, which is
%%%   a list of events to send to the client (atom `nothing' in the place has
%%%   the same result as returning empty list).
%%%
%%%   <ul>
%%%     <li>
%%%       `init(RootURI :: uri(), URI :: uri(), Headers :: [http_header()],
%%%             Args :: term())'
%%%       <ul>
%%%         <li>{@type @{ok, term()@}}</li>
%%%         <li>{@type @{ok, term(), timeout()@}}</li>
%%%         <li>{@type @{ok, term(), hibernate@}}</li>
%%%         <li>{@type @{error, term()@}}</li>
%%%       </ul>
%%%     </li>
%%%     <li>
%%%       `terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
%%%                  State :: term())'
%%%       <ul>
%%%         <li>returned value is ignored</li>
%%%       </ul>
%%%     </li>
%%%     <li>
%%%       `handle_call(Request :: term(), From :: {pid(), Tag :: term()},
%%%                    State :: term())'
%%%       <ul>
%%%         <li>{@type @{reply, Reply :: term(), nothing | [event()], NewState :: term()@}}</li>
%%%         <li>{@type @{reply, Reply :: term(), nothing | [event()], NewState :: term(), timeout()@}}</li>
%%%         <li>{@type @{reply, Reply :: term(), nothing | [event()], NewState :: term(), hibernate@}}</li>
%%%         <li>{@type @{noreply, nothing | [event()], NewState :: term()@}}</li>
%%%         <li>{@type @{noreply, nothing | [event()], NewState :: term(), timeout()@}}</li>
%%%         <li>{@type @{noreply, nothing | [event()], NewState :: term(), hibernate@}}</li>
%%%         <li>{@type @{stop, Reason :: term(), Reply :: term(), nothing | [event()], NewState :: term()@}}</li>
%%%         <li>{@type @{stop, Reason :: term(), nothing | [event()], NewState :: term()@}}</li>
%%%       </ul>
%%%     </li>
%%%     <li>
%%%       `handle_cast(Request :: term(), State :: term())'
%%%       <ul>
%%%         <li>{@type @{noreply, nothing | [event()], NewState :: term()@}}</li>
%%%         <li>{@type @{noreply, nothing | [event()], NewState :: term(), timeout()@}}</li>
%%%         <li>{@type @{noreply, nothing | [event()], NewState :: term(), hibernate@}}</li>
%%%         <li>{@type @{stop, Reason :: term(), nothing | [event()], NewState :: term()@}}</li>
%%%       </ul>
%%%     </li>
%%%     <li>
%%%       `handle_info(Message :: term(), State :: term())'
%%%       <ul>
%%%         <li>{@type @{noreply, nothing | [event()], NewState :: term()@}}</li>
%%%         <li>{@type @{noreply, nothing | [event()], NewState :: term(), timeout()@}}</li>
%%%         <li>{@type @{noreply, nothing | [event()], NewState :: term(), hibernate@}}</li>
%%%         <li>{@type @{stop, Reason :: term(), nothing | [event()], NewState :: term()@}}</li>
%%%       </ul>
%%%     </li>
%%%     <li>
%%%       `code_change(OldVsn :: term() | {down, term()}, State :: term(),
%%%                    Extra :: term())'
%%%       <ul>
%%%         <li>{@type @{ok, NewState :: term()@}}</li>
%%%         <li>{@type @{error, term()@}}</li>
%%%       </ul>
%%%       <b>Note</b>: since I don't have much experience with Erlang
%%%       releases, this function most probably doesn't work correctly (this
%%%       is a working example of cargo cult programming).
%%%     </li>
%%%   </ul>
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_sse_server).

-export([behaviour_info/1]).

-export_type([event/0, uri/0, http_header/0]).

%%%---------------------------------------------------------------------------

-type uri() :: string().
%% Path from the request (or module mountpoint).

-type event() :: iolist().
%% Event to be sent. This should be a bytestring ready to pass through
%% network, so `unicode:characters_to_binary/2' could be handy.

-type http_header() :: {term(), term()}.
%% Single HTTP request header.

%%%---------------------------------------------------------------------------

behaviour_info(callbacks) ->
  [{init, 4}, {terminate, 2},
    {handle_call, 3}, {handle_cast, 2}, {handle_info, 2},
    {code_change, 3}];
behaviour_info(_Any) ->
  undefined.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker:nowrap
