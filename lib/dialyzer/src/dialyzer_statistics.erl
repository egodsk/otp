-module(dialyzer_statistics).
-behavior(gen_server).
% - for internal use
% - for internal use
-export([init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2, code_change/3]).
% - quick halt (note: casts are Asynchronous)
-export([stop/0]).

% - for external use (note: calls are Synchronous)
-export([start/0, increment_counter_any_succ/0, increment_counter_any_contract/0, increment_counter_call/0, increment_counter_cast/0,
  increment_counter_call_lookup_failed/0, increment_counter_cast_lookup_failed/0, get_statistics/0]).

% Client APIs
-spec increment_counter_any_succ() -> any().
increment_counter_any_succ() ->
  gen_server:cast(?MODULE, {increment, any_succ}).

-spec increment_counter_any_contract() -> any().
increment_counter_any_contract() ->
  gen_server:cast(?MODULE, {increment, any_contract}).

-spec increment_counter_call() -> any().
increment_counter_call() ->
  gen_server:cast(?MODULE, {increment, call}).

-spec increment_counter_cast() -> any().
increment_counter_cast() ->
  gen_server:cast(?MODULE, {increment, cast}).

-spec increment_counter_call_lookup_failed() -> any().
increment_counter_call_lookup_failed() ->
  gen_server:cast(?MODULE, {increment, call_lookup_failed}).

-spec increment_counter_cast_lookup_failed() -> any().
increment_counter_cast_lookup_failed() ->
  gen_server:cast(?MODULE, {increment, cast_lookup_failed}).

-spec get_statistics() -> any().
get_statistics() ->
  gen_server:call(?MODULE, get_statistics).

-spec start() -> any().
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% Server implementation
-spec init(any()) -> any().
init([]) -> Dictionary = dict:from_list(
  [
    {any_succ, 0},
    {any_contract, 0},
    {call, 0},
    {cast, 0},
    {call_lookup_failed, 0},
    {cast_lookup_failed, 0}
  ]),
  {ok, Dictionary}.

-spec handle_cast(any(), any()) -> any().
handle_cast({increment, Who}, Dictionary) ->
  Dict2 = dict:update(Who, fun(Val) -> Val + 1 end, Dictionary),
  {noreply, Dict2}.

-spec handle_call(any(), any(), any()) -> any().
handle_call(get_statistics, _From, Dictionary) ->
  AnySucc = dict:fetch(any_succ, Dictionary),
  AnyContract = dict:fetch(any_contract, Dictionary),
  Call = dict:fetch(call, Dictionary),
  Cast = dict:fetch(cast, Dictionary),
  CallLookupFailed = dict:fetch(call_lookup_failed, Dictionary),
  CastLookupFailed = dict:fetch(cast_lookup_failed, Dictionary),

  Res = [{any_succ, AnySucc}, {any_contract, AnyContract}, {call, Call},
    {cast, Cast}, {call_lookup_failed, CallLookupFailed}, {cast_lookup_failed, CastLookupFailed}],

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
