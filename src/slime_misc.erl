-module(slime_misc).

-export([
  optional/1,
  optional/2,

  any/1,
  all/1,
  sub/1,

  integer/1,
  float/1,
  binary/1,
  iso8601_date/1,
  iso8601_datetime/1,
  iso8601_datetimems/1,
  compare/1,
  length/1,
  string_length/1
]).

-include("slime.hrl").


-spec optional(rule()) -> rule().
optional(Rule) ->
  fun(Value) -> do_optional(Rule, Value) end.


-spec optional(rule(), any()) -> rule().
optional(Rule, Default) ->
  fun(Value) -> do_optional(Rule, Value, Default) end.


-spec any([rule()]) -> rule().
any(Rules) ->
  fun(Value) -> any(Rules, Value, []) end.


-spec all([rule()]) -> rule().
all(Rules) ->
  fun(Value) -> all(Rules, Value) end.


-spec sub(rules()) -> rule().
sub(Rules) ->
  fun(Value) -> sub(Rules, Value) end.


-spec integer(any()) -> {ok, integer()} | {error, error()}.
integer(undefined) ->
  {error, undefined};
integer(Value) when is_integer(Value) ->
  {ok, Value};
integer(Value) when is_binary(Value) ->
  try {ok, binary_to_integer(Value)}
  catch _:_ -> {error, {not_integer, Value}}
  end;
integer(Value) when is_list(Value) ->
  try {ok, list_to_integer(Value)}
  catch _:_ -> {error, {not_integer, Value}}
  end;
integer(Value) ->
  {error, {not_integer, Value}}.


-spec float(any()) -> {ok, float()} | {error, error()}.
float(undefined) ->
  {error, undefined};
float(Value) when is_float(Value) ->
  {ok, Value};
float(Value) when is_binary(Value) ->
  try {ok, binary_to_float(Value)}
  catch _:_ -> {error, {not_float, Value}}
  end;
float(Value) when is_list(Value) ->
  try {ok, list_to_float(Value)}
  catch _:_ -> {error, {not_float, Value}}
  end;
float(Value) ->
  {error, {not_float, Value}}.


-spec binary(any()) -> {ok, binary()} | {error, error()}.
binary(undefined) ->
  {error, undefined};
binary(Value) when is_binary(Value) ->
  {ok, Value};
binary(Value) when is_list(Value) ->
  {ok, iolist_to_binary(Value)};
binary(Value) ->
  {error, {not_binary, Value}}.


-spec iso8601_date(any()) -> {ok, calendar:date()} | {error, error()}.
iso8601_date(Value) when not is_binary(Value) ->
  case binary(Value) of
    {ok, Value2} -> iso8601_date(Value2);
    Any -> Any
  end;
iso8601_date(Value) ->
  try iso8601:parse(Value) of
    DateTime -> {ok, DateTime}
  catch
    _:_ -> {error, {not_date, Value}}
  end.


-spec iso8601_datetime(any()) -> {ok, calendar:datetime()} | {error, error()}.
iso8601_datetime(Value) when not is_binary(Value) ->
  case binary(Value) of
    {ok, Value2} -> iso8601_date(Value2);
    Any -> Any
  end;
iso8601_datetime(Value) ->
  try iso8601:parse(Value) of
    DateTime -> {ok, DateTime}
  catch
    _:_ -> {error, {not_datetime, Value}}
  end.


-spec iso8601_datetimems(any()) -> {ok, iso8601:datetime()} | {error, error()}.
iso8601_datetimems(Value) when not is_binary(Value) ->
  case binary(Value) of
    {ok, Value2} -> iso8601_date(Value2);
    Any -> Any
  end;
iso8601_datetimems(Value) ->
  try iso8601:parse_exact(Value) of
    DateTime -> {ok, DateTime}
  catch
    _:_ -> {error, {not_datetimems, Value}}
  end.


-spec compare(compare()) -> rule().
compare(Compare) ->
  fun(Value) -> compare(Compare, Value) end.


-spec length(compare()) -> rule().
length(Compare) ->
  fun(Value) -> length(Compare, Value) end.


-spec string_length(compare()) -> rule().
string_length(Compare) ->
  fun(Value) -> string_length(Compare, Value) end.


-spec do_optional(rule(), any() | undefined) -> {ok, any()} | {error, error()}.
do_optional(_Rule, undefined) ->
  undefined;
do_optional(Rule, Value) ->
  Rule(Value).


-spec do_optional(rule(), any() | undefined, any()) -> {ok, any()} | {error, error()}.
do_optional(Rule, undefined, Default) ->
  Rule(Default);
do_optional(Rule, Value, _) ->
  Rule(Value).


-spec any([rule()], any(), [error()]) -> {ok, any()} | {error, [error()]}.
any([], _Value, Errors) ->
  {error, {non_of, Errors}};
any([Rule|Rules], Value, Errors) ->
  case Rule(Value) of
    {ok, Value2} -> {ok, Value2};
    {error, Error} -> any(Rules, Value, Errors ++ [Error])
  end.


-spec all([rule()], any()) -> {ok, any()} | {error, error()}.
all([], Value) ->
  {ok, Value};
all([Rule|Rules], Value) ->
  case Rule(Value) of
    {ok, Value2} -> all(Rules, Value2);
    Error -> Error
  end.


-spec sub(rules(), map()) -> rule().
sub(Rules, Data) ->
  slime:validate(Rules, Data).


-spec length(compare(), any()) -> {ok, any()} | {error, error()}.
length(_, undefined) ->
  {error, undefined};
length(Compare, Value) when is_binary(Value) ->
  case compare(Compare, byte_size(Value)) of
    {ok, _} -> {ok, Value};
    Error -> Error
  end;
length(Compare, Value) when is_list(Value) ->
  case compare(Compare, erlang:length(Value)) of
    {ok, _} -> {ok, Value};
    Error -> Error
  end;
length(_Compare, Value) ->
  {error, {wrong_value, Value}}.


-spec string_length(compare(), any()) -> {ok, any()} | {error, error()}.
string_length(_, undefined) ->
  {error, undefined};
string_length(Compare, Value) when is_binary(Value) ->
  case compare(Compare, string:length(Value)) of
    {ok, _} -> {ok, Value};
    Error -> Error
  end;
string_length(Compare, Value) ->
  length(Compare, Value).


-spec compare(compare(), integer()) -> {ok, integer()} | {error, error()}.
compare(_, undefined) ->
  {error, undefined};
compare({eq, V}, Value) ->
  compare(Value == V, Value, V, not_equal);
compare({neq, V}, Value) ->
  compare(Value =/= V, Value, V, equal);
compare({gt, V}, Value) ->
  compare(Value > V, Value, V, not_greater);
compare({gte, V}, Value) ->
  compare(Value >= V, Value, V, not_greater_or_equal);
compare({lt, V}, Value) ->
  compare(Value < V, Value, V, not_less);
compare({lte, V}, Value) ->
  compare(Value =< V, Value, V, not_less_or_equal);
compare({between, {From, To} = V}, Value) ->
  compare((Value >= From) and (Value =< To), Value, V, not_between);
compare({in, V}, Value) ->
  compare(lists:member(Value, V), Value, V, not_in).


-spec compare(boolean(), any(), any(), atom()) -> {ok, any()} | {error, {atom(), {any(), any()}}}.
compare(true, Value, _V, _Error) ->
  {ok, Value};
compare(false, Value, V, Error) ->
  {error, {Error, {Value, V}}}.