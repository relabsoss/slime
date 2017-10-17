-module(slime).

-export([
  validate/2,
  any/1,
  all/1,
  integer/1,
  float/1,
  binary/1,
  string_length/1,
  length/1,
  compare/1
]).


-type error() :: binary() | atom() | {atom(), any()}.

-type errors() ::
  error() |
  {
    #{atom => error()},
    error()
  }.

-type rule() :: fun(( any() )-> {ok, any()} | {error, error()}).

-type property_rules() :: #{atom() => rule() | property_rules()}.

-type post_rule() :: fun(( any() )-> {ok, any()} | {error, errors()}).

-type rules() :: {property_rules(), post_rule() | undefined} | property_rules().

-type data() :: proplists:proplist() | map().

-type compare() :: {eq | neq | gt | gte | lt | lte, integer()} | {between, {integer(), integer()}} | {in, [integer()]}.


-spec validate(rules(), data()) -> {ok, map()} | {error, errors()}.
validate(PropertyRules, Data) when is_map(PropertyRules) ->
  do_validate({PropertyRules, undefined}, Data);
validate({PropertyRules, undefined} = Rules, Data) when is_map(PropertyRules) ->
  do_validate(Rules, Data);
validate({PropertyRules, Post} = Rules, Data) when is_map(PropertyRules), is_function(Post) ->
  do_validate(Rules, Data);
validate(Rules, _Data) ->
  {error, {wrong_rules, Rules}}.


-spec any([rule()]) -> rule().
any(Rules) ->
  fun(Value) -> any(Rules, Value, []) end.


-spec all([rule()]) -> rule().
all(Rules) ->
  fun(Value) -> all(Rules, Value) end.


-spec integer(any()) -> {ok, integer()} | {error, error()}.
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
binary(Value) when is_binary(Value) ->
  {ok, Value};
binary(Value) when is_list(Value) ->
  {ok, iolist_to_binary(Value)};
binary(Value) ->
  {error, {not_binary, Value}}.


-spec compare(compare()) -> rule().
compare(Compare) ->
  fun(Value) -> compare(Compare, Value) end.


-spec length(compare()) -> rule().
length(Compare) ->
  fun(Value) -> length(Compare, Value) end.


-spec string_length(compare()) -> rule().
string_length(Compare) ->
  fun(Value) -> string_length(Compare, Value) end.


do_validate({PropertyRules, Post}, Data) ->
  case do_validate_properties(PropertyRules, Data) of
    {ok, Data2} ->
      do_validate_post(Post, Data2);
    Errors -> Errors
  end.


do_validate_properties(Rules, Data) when is_list(Data) ->
  do_validate_properties(Rules, maps:from_list(Data));
do_validate_properties(Rules, Data) when is_map(Data) ->
  do_validate_properties(maps:to_list(Rules), #{}, #{}, Data);
do_validate_properties(_Rules, Data) ->
  {error, {wrong_data, Data}}.


do_validate_properties([], ValidData, #{}, _) ->
  {ok, ValidData};
do_validate_properties([], _, Errors, _) ->
  {error, Errors};
do_validate_properties([{K, Rule}|Rules], ValidData, Errors, Data) when is_function(Rule) ->
  case get_value(K, Data) of
    {ok, Value} ->
      case Rule(Value) of
        {ok, V} ->
          do_validate_properties(Rules, ValidData#{ K => V }, Errors, Data);
        Error ->
          do_validate_properties(Rules, ValidData, Errors#{ K => Error }, Data)
      end;
    {error, Reason} ->
      do_validate_properties(Rules, ValidData, Errors#{ K => Reason }, Data);
    Error ->
      do_validate_properties(Rules, ValidData, Errors#{ K => Error }, Data)
  end;
do_validate_properties([{K, SubRules}|Rules], ValidData, Errors, Data) when is_map(SubRules) ->
  case get_value(K, Data) of
    {ok, SubData} ->
      case do_validate_properties(SubRules, SubData) of
        {ok, SubData2} ->
          do_validate_properties(Rules, ValidData#{ K => SubData2}, Errors, Data);
        Error ->
          do_validate_properties(Rules, ValidData, Errors#{ K => Error }, Data)
      end;
    {error, Reason} ->
      do_validate_properties(Rules, ValidData, Errors#{ K => Reason }, Data);
    Error ->
      do_validate_properties(Rules, ValidData, Errors#{ K => Error }, Data)
  end;
do_validate_properties([{K, Rule}|Rules], ValidData, Errors, Data) ->
  do_validate_properties(Rules, ValidData, Errors#{ K => {unknown_rule, Rule} }, Data).


do_validate_post(undefined, Data) ->
  {ok, Data};
do_validate_post(Rule, Data) ->
  Rule(Data).


get_value(K, Data) when is_atom(K) ->
  get_value([K, atom_to_binary(K, utf8), atom_to_list(K)], Data);
get_value([], _Data) ->
  {error, undefined};
get_value([K|Keys], Data) ->
  case maps:get(K, Data, undefined) of
    undefined -> get_value(Keys, Data);
    Value -> {ok, Value}
  end.


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


-spec length(compare(), any()) -> {ok, any()} | {error, error()}.
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
string_length(Compare, Value) when is_binary(Value) ->
  case compare(Compare, string:length(Value)) of
    {ok, _} -> {ok, Value};
    Error -> Error
  end;
string_length(Compare, Value) ->
  length(Compare, Value).


-spec compare(compare(), integer()) -> {ok, integer()} | {error, error()}.
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