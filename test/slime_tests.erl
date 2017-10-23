-module(slime_tests).

-include_lib("eunit/include/eunit.hrl").


float_test_() ->
  [
    ?_assertEqual(
      {error, {not_float, 1}},
      slime_misc:float(1)),
    ?_assertEqual(
      {error, {not_float, "1"}},
      slime_misc:float("1")),
    ?_assertEqual(
      {error, {not_float, <<"1">>}},
      slime_misc:float(<<"1">>)),
    ?_assertEqual(
      {ok, 1.1},
      slime_misc:float(1.1)),
    ?_assertEqual(
      {ok, 1.1},
      slime_misc:float("1.1")),
    ?_assertEqual(
      {ok, 1.1},
      slime_misc:float(<<"1.1">>)),
    ?_assertEqual(
      {error, {not_float, <<"not_float">>}},
      slime_misc:float(<<"not_float">>))].


integer_test_() ->
  [
    ?_assertEqual(
      {ok, 1},
      slime_misc:integer(1)),
    ?_assertEqual(
      {ok, 1},
      slime_misc:integer("1")),
    ?_assertEqual(
      {ok, 1},
      slime_misc:integer(<<"1">>)),
    ?_assertEqual(
      {error, {not_integer, <<"not_integer">>}},
      slime_misc:integer(<<"not_integer">>))].


compare_test_() ->
  [
    ?_assertEqual(
      {ok, 3},
      (slime_misc:compare({eq, 3}))
        (3)),
    ?_assertEqual(
      {ok, 3},
      (slime_misc:compare({neq, 4}))
        (3)),

    ?_assertEqual(
      {ok, 3},
      (slime_misc:compare({gt, 2}))
        (3)),
    ?_assertEqual(
      {ok, 3},
      (slime_misc:compare({gte, 3}))
        (3)),
    ?_assertEqual(
      {error, {not_greater, {3, 3}}},
      (slime_misc:compare({gt, 3}))
        (3)),
    ?_assertEqual(
      {error, {not_greater_or_equal, {3, 4}}},
      (slime_misc:compare({gte, 4}))
        (3)),

    ?_assertEqual(
      {ok, 3},
      (slime_misc:compare({lt, 4}))
        (3)),
    ?_assertEqual(
      {ok, 3},
      (slime_misc:compare({lte, 3}))
        (3)),
    ?_assertEqual(
      {error, {not_less, {3, 3}}},
      (slime_misc:compare({lt, 3}))
        (3)),
    ?_assertEqual(
      {error, {not_less_or_equal, {3, 2}}},
      (slime_misc:compare({lte, 2}))
        (3)),

    ?_assertEqual(
      {ok, 3},
      (slime_misc:compare({between, {1, 3}}))
        (3)),
    ?_assertEqual(
      {error, {not_between, {3, {1, 2}}}},
      (slime_misc:compare({between, {1, 2}}))
        (3)),

    ?_assertEqual(
      {ok, 3},
      (slime_misc:compare({in, [1, 2, 3]}))
        (3)),
    ?_assertEqual(
      {error, {not_in, {3, [1, 2]}}},
      (slime_misc:compare({in, [1, 2]}))
        (3))
  ].


length_test_() ->
  [
    ?_assertEqual(
      {ok, [1,2,3]},
      (slime_misc:length({eq, 3}))
        ([1,2,3])),
    ?_assertEqual(
      {ok, "text"},
      (slime_misc:length({eq, 4}))
        ("text")),
    ?_assertEqual(
      {ok, <<"text">>},
      (slime_misc:length({eq, 4}))
        (<<"text">>)),
    ?_assertEqual(
      {ok, <<"тест"/utf8>>},
      (slime_misc:length({eq, 8}))
      (<<"тест"/utf8>>)),
    ?_assertEqual(
      {ok, <<"тест"/utf8>>},
      (slime_misc:string_length({eq, 4}))
        (<<"тест"/utf8>>)),
    ?_assertEqual(
      {error, {wrong_value, 1}},
      (slime_misc:length({eq, 4}))
        (1))
  ].


all_test_() ->
  [
    ?_assertEqual(
      {ok, <<"text">>},
      (slime_misc:all([fun slime_misc:binary/1, slime_misc:length({gt, 2})]))
        (<<"text">>)),
    ?_assertEqual(
      {error, {not_greater, {4, 5}}},
      (slime_misc:all([fun slime_misc:binary/1, slime_misc:length({gt, 5})]))
       (<<"text">>))
  ].


any_test_() ->
  [
    ?_assertEqual(
      {ok, <<"text">>},
      (slime_misc:any([fun slime_misc:binary/1, fun slime_misc:integer/1]))
        (<<"text">>)),

    ?_assertEqual(
      {ok, 1},
      (slime_misc:any([fun slime_misc:binary/1, fun slime_misc:integer/1]))
        (1)),

    ?_assertEqual(
      {error, {non_of, [{not_binary, 1.0}, {not_integer, 1.0}]}},
      (slime_misc:any([fun slime_misc:binary/1, fun slime_misc:integer/1]))
        (1.0))
  ].


optional_test_() ->
  [
    ?_assertEqual(
      {ok, #{fieldA => 1, fieldB => <<"text">>}},
      slime:validate(
        #{
          fieldA => slime_misc:optional(fun slime_misc:integer/1, 1),
          fieldB => fun slime_misc:binary/1
        }, #{
          fieldB => "text"
        })),

    ?_assertEqual(
      {ok, #{fieldB => <<"text">>}},
      slime:validate(
        #{
          fieldA => slime_misc:optional(fun slime_misc:integer/1),
          fieldB => fun slime_misc:binary/1
        }, #{
          fieldB => "text"
        }))
  ].


validate_test_() ->
  [
    ?_assertEqual(
      {ok, #{fieldA => 1, fieldB => <<"text">>}},
      slime:validate(
        #{
          fieldA => slime_misc:optional(fun slime_misc:integer/1),
          fieldB => fun slime_misc:binary/1
        }, #{
        fieldA => 1,
        fieldB => "text"
      })),

    ?_assertEqual(
      {ok, #{fieldA => 1, fieldB => <<"text">>}},
      slime:validate(
        #{
          fieldA => fun slime_misc:integer/1,
          fieldB => fun slime_misc:binary/1
        }, #{
        "fieldA" => 1,
        <<"fieldB">> => "text"
      })),

    ?_assertEqual(
      {ok, #{fieldA => 1, fieldB => <<"text">>, fieldC => #{ fieldD => 1.0 }}},
      slime:validate(
        #{
          fieldA => fun slime_misc:integer/1,
          fieldB => fun slime_misc:binary/1,
          fieldC => slime_misc:sub(#{
            fieldD => fun slime_misc:float/1
          })
        }, #{
        "fieldA" => 1,
        <<"fieldB">> => "text",
        fieldC => #{
          "fieldD" => 1.0
        }
      })),

    ?_assertEqual(
      {ok, #{fieldA => 1, fieldB => <<"text">>, fieldC => #{ fieldD => 1.0 }}},
      slime:validate({
        #{
          fieldA => fun slime_misc:integer/1,
          fieldB => fun slime_misc:binary/1,
          fieldC => slime_misc:sub(#{
            fieldD => fun slime_misc:float/1
          })
        },
        fun
          (#{ fieldA := 1 } = Value) -> {ok, Value};
          (_) -> {error, {#{}, wrong_fieldA}}
        end
      }, #{
        "fieldA" => 1,
        <<"fieldB">> => "text",
        fieldC => #{
          "fieldD" => 1.0
        }
      })),

    ?_assertEqual(
      {error, {#{}, wrong_fieldA}},
      slime:validate({
        #{
          fieldA => fun slime_misc:integer/1,
          fieldB => fun slime_misc:binary/1,
          fieldC => slime_misc:sub(#{
            fieldD => fun slime_misc:float/1
          })
        },
        fun
          (#{ fieldA := 2 } = Value) -> {ok, Value};
          (_) -> {error, {#{}, wrong_fieldA}}
        end
      }, #{
        "fieldA" => 1,
        <<"fieldB">> => "text",
        fieldC => #{
          "fieldD" => 1.0
        }
      }))
  ].


validate_map_test_() ->
  [
    ?_assertEqual(
      {ok, #{fieldA => 1}},
      slime:validate(#{
        fieldA => fun slime_misc:integer/1
      }, #{
        "fieldA" => 1
      })),

    ?_assertEqual(
      {ok, #{fieldB => 1}},
      slime:validate(#{
        {fieldA, fieldB} => fun slime_misc:integer/1
      }, #{
        "fieldA" => 1
      })),

    ?_assertEqual(
      {ok, #{fieldA => 1, fieldC => 1}},
      slime:validate(#{
        fieldA => fun slime_misc:integer/1,
        {fieldB, fieldC} => fun slime_misc:integer/1
      }, #{
        "fieldA" => 1,
        "fieldB" => 1
      }))
  ].