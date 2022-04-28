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
  typesig_increment_call_arity_lookup/0, typesig_increment_call_mfa_lookup/0, typesig_increment_call_generic/0,
  typesig_increment_cast_mfa_lookup/0, typesig_increment_cast_generic/0,
  typesig_increment_known_bug/0,
  dataflow_increment_call_arity_lookup/0, dataflow_increment_call_mfa_lookup/0, dataflow_increment_call_generic/0,
  dataflow_increment_cast_mfa_lookup/0, dataflow_increment_cast_generic/0
]).

% Client APIs

%% TYPESIG
%% CALL
-spec typesig_increment_call_arity_lookup() -> ok.
typesig_increment_call_arity_lookup() -> gen_server:cast(?MODULE, {typesig_increment, call_arity_lookup}).

-spec typesig_increment_call_mfa_lookup() -> ok.
typesig_increment_call_mfa_lookup() -> gen_server:cast(?MODULE, {typesig_increment, call_mfa_lookup}).

-spec typesig_increment_call_generic() -> ok.
typesig_increment_call_generic() -> gen_server:cast(?MODULE, {typesig_increment, call_generic}).

%% CAST
-spec typesig_increment_cast_mfa_lookup() -> ok.
typesig_increment_cast_mfa_lookup() -> gen_server:cast(?MODULE, {typesig_increment, cast_mfa_lookup}).

-spec typesig_increment_cast_generic() -> ok.
typesig_increment_cast_generic() -> gen_server:cast(?MODULE, {typesig_increment, cast_generic}).

%% BUG
-spec typesig_increment_known_bug() -> ok.
typesig_increment_known_bug() -> gen_server:cast(?MODULE, {typesig_increment, known_bug}).

%% DATAFLOW
%% CALL
-spec dataflow_increment_call_arity_lookup() -> ok.
dataflow_increment_call_arity_lookup() -> gen_server:cast(?MODULE, {dataflow_increment, call_arity_lookup}).

-spec dataflow_increment_call_mfa_lookup() -> ok.
dataflow_increment_call_mfa_lookup() -> gen_server:cast(?MODULE, {dataflow_increment, call_mfa_lookup}).

-spec dataflow_increment_call_generic() -> ok.
dataflow_increment_call_generic() -> gen_server:cast(?MODULE, {dataflow_increment, call_generic}).

%% CAST
-spec dataflow_increment_cast_mfa_lookup() -> ok.
dataflow_increment_cast_mfa_lookup() -> gen_server:cast(?MODULE, {dataflow_increment, cast_mfa_lookup}).

-spec dataflow_increment_cast_generic() -> ok.
dataflow_increment_cast_generic() -> gen_server:cast(?MODULE, {dataflow_increment, cast_generic}).

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
    {typesig_call_arity_lookup, 0},
    {typesig_call_mfa_lookup, 0}, {typesig_cast_mfa_lookup, 0},
    {typesig_call_generic, 0}, {typesig_cast_generic, 0},
    {known_bug, 0},

    {dataflow_call_arity_lookup, 0},
    {dataflow_call_mfa_lookup, 0}, {dataflow_cast_mfa_lookup, 0},
    {dataflow_call_generic, 0}, {dataflow_cast_generic, 0}
  ]),
  {ok, Dictionary}.

-spec handle_cast(any(), any()) -> any().
handle_cast({increment, Who}, Dictionary) ->
  Dict2 = dict:update(Who, fun(Val) -> Val + 1 end, Dictionary),
  {noreply, Dict2}.

-spec handle_call(get_statistics, any(), any()) -> any().
handle_call(get_statistics, _From, Dictionary) ->
  Typesig_Call_Arity_Lookup = dict:fetch(typesig_call_arity_lookup, Dictionary),
  Typesig_Call_Mfa_Lookup = dict:fetch(typesig_call_mfa_lookup, Dictionary),
  Typesig_Call_Generic = dict:fetch(typesig_call_generic, Dictionary),

  Typesig_Cast_Mfa_Lookup = dict:fetch(typesig_cast_mfa_lookup, Dictionary),
  Typesig_Cast_Generic = dict:fetch(typesig_cast_generic, Dictionary),

  Known_Bug = dict:fetch(known_bug, Dictionary),

  Dataflow_Call_Arity_Lookup = dict:fetch(dataflow_call_arity_lookup, Dictionary),
  Dataflow_Call_Mfa_Lookup = dict:fetch(dataflow_call_mfa_lookup, Dictionary),
  Dataflow_Call_Generic = dict:fetch(dataflow_call_generic, Dictionary),

  Dataflow_Cast_Mfa_Lookup = dict:fetch(dataflow_cast_mfa_lookup, Dictionary),
  Dataflow_Cast_Generic = dict:fetch(dataflow_cast_generic, Dictionary),

  Res = [
    {typesig_call_arity_lookup, Typesig_Call_Arity_Lookup},
    {typesig_call_mfa_lookup, Typesig_Call_Mfa_Lookup},
    {typesig_call_generic, Typesig_Call_Generic},

    {typesig_cast_mfa_lookup, Typesig_Cast_Mfa_Lookup},
    {typesig_cast_generic, Typesig_Cast_Generic},

    {dataflow_call_arity_lookup, Dataflow_Call_Arity_Lookup},
    {dataflow_call_mfa_lookup, Dataflow_Call_Mfa_Lookup},
    {dataflow_call_generic, Dataflow_Call_Generic},

    {dataflow_cast_mfa_lookup, Dataflow_Cast_Mfa_Lookup},
    {dataflow_cast_generic, Dataflow_Cast_Generic},


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
