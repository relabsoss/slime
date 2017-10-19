-module(slime).


-export([
  validate/2
]).

-export_type([
  error/0,
  errors/0,
  rule/0,
  property_rules/0,
  rules/0,
  data/0,
  compare/0
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

-type compare() :: {eq | neq | gt | gte | lt | lte, integer()} | {between, {integer(), integer()}} | {in, [any()]}.


%% @doc Validate Data with Rules
-spec validate(Rules :: rules(), Data :: data()) -> {ok, map()} | {error, errors()}.
validate(PropertyRules, Data) when is_map(PropertyRules) ->
  do_validate({PropertyRules, undefined}, Data);
validate({PropertyRules, undefined} = Rules, Data) when is_map(PropertyRules) ->
  do_validate(Rules, Data);
validate({PropertyRules, Post} = Rules, Data) when is_map(PropertyRules), is_function(Post) ->
  do_validate(Rules, Data);
validate(Rules, _Data) ->
  {error, {wrong_rules, Rules}}.


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
  case Rule(get_value(K, Data)) of
    undefined ->
      do_validate_properties(Rules, ValidData, Errors, Data);
    {ok, V} ->
      do_validate_properties(Rules, ValidData#{ K => V }, Errors, Data);
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
  undefined;
get_value([K|Keys], Data) ->
  case maps:get(K, Data, undefined) of
    undefined -> get_value(Keys, Data);
    Value -> Value
  end.