-module(dialyzer_statistics).
-behavior(gen_server).
% - for internal use
% - for internal use
-export([init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2, code_change/3]).
% - quick halt (note: casts are Asynchronous)
-export([stop/0]).

% - for external use (note: calls are Synchronous)
-export([start/0, increment_counter_any_succ/1, increment_counter_any_contract/1, increment_counter_call/1, increment_counter_cast/1,
  increment_counter_call_arity/1,
  increment_known_none_bug/1,
  increment_counter_call_arity_lookup_failed/1, increment_counter_call_lookup_failed/1, increment_counter_cast_lookup_failed/1, get_statistics/0, get_new_statistics/0]).

% Client APIs

% Any in success typing
-spec increment_counter_any_succ(any()) -> any().
increment_counter_any_succ(dialyzer_typesig) ->
  gen_server:cast(?MODULE, {increment, any_succ});
increment_counter_any_succ(dialyzer_dataflow) ->
  gen_server:cast(?MODULE, {increment, any_succ_dataflow}).

% Known bug
-spec increment_known_none_bug(any()) -> any().
increment_known_none_bug(dialyzer_typesig) ->
  gen_server:cast(?MODULE, {increment, known_bug}).

% Any in contract
-spec increment_counter_any_contract(any()) -> any().
increment_counter_any_contract(dialyzer_typesig) ->
  gen_server:cast(?MODULE, {increment, any_contract});
increment_counter_any_contract(dialyzer_dataflow) ->
  gen_server:cast(?MODULE, {increment, any_contract_dataflow}).

% genserver:call with arity used
-spec increment_counter_call_arity(any()) -> any().
increment_counter_call_arity(dialyzer_typesig) ->
  gen_server:cast(?MODULE, {increment, call_arity}),
  gen_server:cast(?MODULE, {increment, call_arity_success});
increment_counter_call_arity(dialyzer_dataflow) ->
  gen_server:cast(?MODULE, {increment, call_arity_dataflow}),
  gen_server:cast(?MODULE, {increment, call_arity_success_dataflow}).


% genserver:call type with arity not found in PLT
-spec increment_counter_call_arity_lookup_failed(any()) -> any().
increment_counter_call_arity_lookup_failed(dialyzer_typesig) ->
  gen_server:cast(?MODULE, {increment, call_arity_lookup_failed}),
  gen_server:cast(?MODULE, {increment, call_arity_failed});
increment_counter_call_arity_lookup_failed(dialyzer_dataflow) ->
  gen_server:cast(?MODULE, {increment, call_arity_lookup_failed_dataflow}),
  gen_server:cast(?MODULE, {increment, call_arity_failed_dataflow}).

% genserver:call used
-spec increment_counter_call(any()) -> any().
increment_counter_call(dialyzer_typesig) ->
  gen_server:cast(?MODULE, {increment, call}),
  gen_server:cast(?MODULE, {increment, call_mfa_success});
increment_counter_call(dialyzer_dataflow) ->
  gen_server:cast(?MODULE, {increment, call_dataflow}),
  gen_server:cast(?MODULE, {increment, call_mfa_success_dataflow}).

% genserver:call type not found in PLT
-spec increment_counter_call_lookup_failed(any()) -> any().
increment_counter_call_lookup_failed(dialyzer_typesig) ->
  gen_server:cast(?MODULE, {increment, call_lookup_failed}),
  gen_server:cast(?MODULE, {increment, call_mfa_failed});
increment_counter_call_lookup_failed(dialyzer_dataflow) ->
  gen_server:cast(?MODULE, {increment, call_lookup_failed_dataflow}),
  gen_server:cast(?MODULE, {increment, call_mfa_failed_dataflow}).

% genserver:cast used
-spec increment_counter_cast(any()) -> any().
increment_counter_cast(dialyzer_typesig) ->
  gen_server:cast(?MODULE, {increment, cast});
increment_counter_cast(dialyzer_dataflow) ->
  gen_server:cast(?MODULE, {increment, cast_dataflow}).

% genserver:cast type not found in PLT
-spec increment_counter_cast_lookup_failed(any()) -> any().
increment_counter_cast_lookup_failed(dialyzer_typesig) ->
  gen_server:cast(?MODULE, {increment, cast_lookup_failed});
increment_counter_cast_lookup_failed(dialyzer_dataflow) ->
  gen_server:cast(?MODULE, {increment, cast_lookup_failed_dataflow}).

-spec get_statistics() -> any().
get_statistics() ->
  gen_server:call(?MODULE, get_statistics).
-spec get_new_statistics() -> any().

get_new_statistics() ->
  gen_server:call(?MODULE, get_new_statistics).

-spec start() -> any().
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% Server implementation
-spec init(any()) -> any().
init([]) -> Dictionary = dict:from_list(
  [
    {call_arity_success, 0}, {call_arity_failed, 0},
    {call_mfa_success, 0}, {call_mfa_failed, 0},
    {call_arity_success_dataflow, 0}, {call_arity_failed_dataflow, 0},
    {call_mfa_success_dataflow, 0}, {call_mfa_failed_dataflow, 0},

    {any_succ, 0}, {any_succ_dataflow, 0},
    {known_bug, 0},
    {any_contract, 0}, {any_contract_dataflow, 0},
    {call_arity, 0}, {call_arity_dataflow, 0}, {call, 0}, {call_dataflow, 0},
    {cast, 0}, {cast_dataflow, 0},
    {call_arity_lookup_failed, 0}, {call_arity_lookup_failed_dataflow, 0},
    {call_lookup_failed, 0}, {call_lookup_failed_dataflow, 0},
    {cast_lookup_failed, 0}, {cast_lookup_failed_dataflow, 0}
  ]),
  {ok, Dictionary}.

-spec handle_cast(any(), any()) -> any().
handle_cast({increment, Who}, Dictionary) ->
  Dict2 = dict:update(Who, fun(Val) -> Val + 1 end, Dictionary),
  {noreply, Dict2}.

-spec handle_call(any(), any(), any()) -> any().
handle_call(get_new_statistics, _From, Dictionary) ->
  CallAritySuccess = dict:fetch(call_arity_success, Dictionary),
  CallArityFailed = dict:fetch(call_arity_failed, Dictionary),
  CallMfaSuccess = dict:fetch(call_mfa_success, Dictionary),
  CallMfaFailed = dict:fetch(call_mfa_failed, Dictionary),

  CallAritySuccessDataflow = dict:fetch(call_arity_success_dataflow, Dictionary),
  CallArityFailedDataflow = dict:fetch(call_arity_failed_dataflow, Dictionary),
  CallMfaSuccessDataflow = dict:fetch(call_mfa_success_dataflow, Dictionary),
  CallMfaFailedDataflow = dict:fetch(call_mfa_failed_dataflow, Dictionary),

  Res = [
    {call_arity_success, CallAritySuccess}, {call_arity_failed, CallArityFailed},
    {call_mfa_success, CallMfaSuccess}, {call_mfa_failed, CallMfaFailed},
    {call_arity_success_dataflow, CallAritySuccessDataflow}, {call_arity_failed_dataflow, CallArityFailedDataflow},
    {call_mfa_success_dataflow, CallMfaSuccessDataflow}, {call_mfa_failed_dataflow, CallMfaFailedDataflow}],

  {reply, Res, Dictionary};

handle_call(get_statistics, _From, Dictionary) ->
  AnySucc = dict:fetch(any_succ, Dictionary),
  AnyContract = dict:fetch(any_contract, Dictionary),
  KnownBug = dict:fetch(known_bug, Dictionary),
  CallArity = dict:fetch(call_arity, Dictionary),
  Call = dict:fetch(call, Dictionary),
  Cast = dict:fetch(cast, Dictionary),
  CallArityLookupFailed = dict:fetch(call_arity_lookup_failed, Dictionary),
  CallLookupFailed = dict:fetch(call_lookup_failed, Dictionary),
  CastLookupFailed = dict:fetch(cast_lookup_failed, Dictionary),

  AnySuccDf = dict:fetch(any_succ_dataflow, Dictionary),
  AnyContractDf = dict:fetch(any_contract_dataflow, Dictionary),
  CallArityDf = dict:fetch(call_arity_dataflow, Dictionary),
  CallDf = dict:fetch(call_dataflow, Dictionary),
  CastDf = dict:fetch(cast_dataflow, Dictionary),
  CallArityLookupFailedDf = dict:fetch(call_arity_lookup_failed_dataflow, Dictionary),
  CallLookupFailedDf = dict:fetch(call_lookup_failed_dataflow, Dictionary),
  CastLookupFailedDf = dict:fetch(cast_lookup_failed_dataflow, Dictionary),

  Res = [
    {any_succ, AnySucc}, {any_contract, AnyContract}, {call_arity, CallArity}, {call, Call},
    {cast, Cast}, {call_arity_lookup_failed, CallArityLookupFailed},
    {known_bug, KnownBug},
    {call_lookup_failed, CallLookupFailed}, {cast_lookup_failed, CastLookupFailed},

    {any_succ_dataflow, AnySuccDf}, {any_contract_dataflow, AnyContractDf}, {call_arity_dataflow, CallArityDf}, {call_dataflow, CallDf},
    {cast_dataflow, CastDf}, {call_arity_lookup_failed_dataflow, CallArityLookupFailedDf},
    {call_lookup_failed_dataflow, CallLookupFailedDf}, {cast_lookup_failed_dataflow, CastLookupFailedDf}],

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
