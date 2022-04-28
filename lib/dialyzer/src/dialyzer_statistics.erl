-module(dialyzer_statistics).
-behavior(gen_server).
% - for internal use
% - for internal use
-export([init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2, code_change/3]).
% - quick halt (note: casts are Asynchronous)
-export([stop/0]).

% - for external use (note: calls are Synchronous)
-export([
  start/0,
  get_statistics/0,
  increment_call_arity_lookup/0, increment_call_mfa_lookup/0, increment_call_generic/0,
  increment_cast_mfa_lookup/0, increment_cast_generic/0,
  increment_known_bug/0
]).

% Client APIs
%% CALL
-spec increment_call_arity_lookup() -> ok.
increment_call_arity_lookup() -> gen_server:cast(?MODULE, {increment, call_arity_lookup}).

-spec increment_call_mfa_lookup() -> ok.
increment_call_mfa_lookup() -> gen_server:cast(?MODULE, {increment, call_mfa_lookup}).

-spec increment_call_generic() -> ok.
increment_call_generic() -> gen_server:cast(?MODULE, {increment, call_generic}).

%% CAST
-spec increment_cast_mfa_lookup() -> ok.
increment_cast_mfa_lookup() -> gen_server:cast(?MODULE, {increment, cast_mfa_lookup}).

-spec increment_cast_generic() -> ok.
increment_cast_generic() -> gen_server:cast(?MODULE, {increment, cast_generic}).

%% BUG
-spec increment_known_bug() -> ok.
increment_known_bug() -> gen_server:cast(?MODULE, {increment, known_bug}).

%% STATISTICS
-spec get_statistics() -> any().
get_statistics() ->
  gen_server:call(?MODULE, get_statistics).

-spec start() -> any().
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% Server implementation
-spec init(any()) -> any().
init([]) -> Dictionary = dict:from_list(
  [
    {call_arity_lookup, 0},
    {call_mfa_lookup, 0}, {cast_mfa_lookup, 0},
    {call_generic, 0}, {cast_generic, 0},

    {known_bug, 0}
  ]),
  {ok, Dictionary}.

-spec handle_cast(any(), any()) -> any().
handle_cast({increment, Who}, Dictionary) ->
  Dict2 = dict:update(Who, fun(Val) -> Val + 1 end, Dictionary),
  {noreply, Dict2}.

-spec handle_call(get_statistics, any(), any()) -> any().
handle_call(get_statistics, _From, Dictionary) ->
  Call_Arity_Lookup = dict:fetch(call_arity_lookup, Dictionary),
  Call_Mfa_Lookup = dict:fetch(call_mfa_lookup, Dictionary),
  Call_Generic = dict:fetch(call_generic, Dictionary),

  Cast_Mfa_Lookup = dict:fetch(cast_mfa_lookup, Dictionary),
  Cast_Generic = dict:fetch(cast_generic, Dictionary),

  Known_Bug = dict:fetch(known_bug, Dictionary),

  Res = [
    {call_arity_lookup, Call_Arity_Lookup},
    {call_mfa_lookup, Call_Mfa_Lookup},
    {call_generic, Call_Generic},

    {cast_mfa_lookup, Cast_Mfa_Lookup},
    {cast_generic, Cast_Generic},

    {known_bug, Known_Bug}
  ],

  {reply, Res, Dictionary}.

-spec stop() -> any().
stop() ->
  gen_server:cast(?MODULE, stop).

-spec terminate(any(), any()) -> any().
terminate(normal, _State) ->
  ok.
%
% - lets keep the compiler quiet with all the call-backs
-spec handle_info(any(), any()) -> any().
handle_info(_Message, Dictionary) -> {noreply, Dictionary}.
-spec code_change(any(), any(), any()) -> any().
code_change(_OldVersion, Dictionary, _Extra) -> {ok, Dictionary}.
