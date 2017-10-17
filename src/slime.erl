-module(slime).

-export([
  validate/2
]).

-include("slime.hrl").


-spec validate(rules(), data()) -> {ok, map()} | {error, errors()}.
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