-module(dialyzer_statistics).
-export([increment_counter_any_succ/0, increment_counter_any_contract/0, increment_counter_call/0, increment_counter_cast/0,
  increment_counter_call_lookup_failed/0, increment_counter_cast_lookup_failed/0, get_statistics/0, print/0]).

update_counter(Key) ->
  case ets:whereis(dialyzer_statistics_table) of
    undefined -> ets:new(dialyzer_statistics_table, [ordered_set, public, named_table]);
    _ -> ok
  end,
  ets:update_counter(dialyzer_statistics_table, Key, 1, {2, 0}).

-spec get_statistics() -> any().
get_statistics() ->
  AnySucc = ets:lookup(dialyzer_statistics_table, any_succ),
  AnyContract = ets:lookup(dialyzer_statistics_table, any_contract),
  Call = ets:lookup(dialyzer_statistics_table, call),
  Cast = ets:lookup(dialyzer_statistics_table, cast),
  CallLookupFailed = ets:lookup(dialyzer_statistics_table, call_lookup_failed),
  CastLookupFailed = ets:lookup(dialyzer_statistics_table, cast_lookup_failed),
  [{any_succ, AnySucc}, {any_contract, AnyContract}, {call, Call},
    {cast, Cast}, {call_lookup_failed, CallLookupFailed}, {cast_lookup_failed, CastLookupFailed}].

-spec print() -> any().
print() ->
  io:format("~p~n", [ets:tab2list(dialyzer_statistics_table)]).

-spec increment_counter_any_succ() -> any().
increment_counter_any_succ() ->
  update_counter(any_succ).

-spec increment_counter_any_contract() -> any().
increment_counter_any_contract() ->
  update_counter(any_contract).

-spec increment_counter_call() -> any().
increment_counter_call() ->
  update_counter(call).

-spec increment_counter_cast() -> any().
increment_counter_cast() ->
  update_counter(cast).

-spec increment_counter_call_lookup_failed() -> any().
increment_counter_call_lookup_failed() ->
  update_counter(call_lookup_failed).

-spec increment_counter_cast_lookup_failed() -> any().
increment_counter_cast_lookup_failed() ->
  update_counter(cast_lookup_failed).




