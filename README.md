# Validation library

```
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

-type compareable() :: integer() | float().

-type compare() :: {eq | neq | gt | gte | lt | lte, compareable()} | {between, {compareable(), compareable()}} | {in, [any()]}.

```

```
validate(Rules :: rules(), Data :: data()) -> {ok, map()} | {error, errors()}.
```

Each property rule trying to get value from Data in order:
- atom
- binary
- list

## Usage

### Basic

```
validate(#{
  fieldA => fun slime_misc:integer/1,
  fieldB => fun slime_misc:float/1,
  fieldC => fun slime_misc:binary/1
}, #{
  fieldA => 10,
  <<"fieldB">> => <<"1.0">>,
  "fieldC" => "text" 
}) == {
  ok, 
  #{
    fieldA => 10,
    fieldB => 1.0,
    fieldC => <<"text">>
  }
}.
```

### Field rename

```
validate(#{
  { fieldA, fieldB } => fun slime_misc:integer/1,
}, #{
  <<"fieldA">> => 10
}) == { ok, #{ fieldB => 10 } }.
```

### Post process on success validation

```
validate({
  #{
    fieldA => fun slime_misc:integer/1,
  },
  fun(#{ fieldA => FieldAValue }) ->
    #{ fieldA => FieldAValue * 100 }
  end
}, #{
  <<"fieldA">> => 10
}) == { ok, #{ fieldA => 10000 } }.
```

### Transform to record on success validation

```
-record(recordA, {fieldA}).

validate({
  #{
    fieldA => fun slime_misc:integer/1,
  },
  fun(#{ fieldA => FieldAValue }) ->
    #recordA{ fieldA => FieldAValue } 
  end
}, #{
  <<"fieldA">> => 10
}) == { ok, #recordA{ fieldA = 10 } }.
```

### Sub map validation

```
slime:validate(#{
  fieldA => fun slime_misc:integer/1,
  fieldB => slime_misc:sub(#{
    fieldC => fun slime_misc:float/1
  })
}, #{
  <<"fieldA">> => 1,
  fieldB => #{
    "fieldC" => 1.0
  }
}) == { 
  ok, 
  #{ 
    fieldA => 1, 
    fieldB => #{ 
      fieldC => 1.0 
    }  
  } 
}.
```

### Optional validation

Property does not exists in result data if property not defined in Data and default value not set.
Otherwise default or validate value assigned to property.


```
validate(#{
  fieldA => fun slime_misc:integer/1,
  fieldB => slime:optional(
    fun slime_misc:float/1),
  fieldC => slime:optional(
    fun slime_misc:binary/1, <<>>)
}, #{
  fieldA => 10
}) == {
  ok, 
  #{
    fieldA => 10,
    fieldC => <<>>
  }
}.
```

## Validation rules

* slime_misc:integer/1 - integer() -> integer(), binary() -> integer(), list() -> integer() 
* slime_misc:float/1 - float() -> float(), binary() -> float(), list() -> float()
* slime_misc:binary/1 - binary() -> binary(), list() -> binary()
* slime_misc:iso_date/1 - binary() -> calendar:date(), list -> calendar:date(). Expecting string format ISO8601
* slime_misc:iso_datetime/1 - binary() -> calendar:datetime(), list -> calendar:datetime(). Expecting string format ISO8601
* slime_misc:iso_datetimems/1 - binary() -> iso8601:datetime(), list -> iso8601:datetime(). Seconds is of type float. Contains milliseconds if present. Expecting string format ISO8601
* slime_misc:any/1 - expect list of rules. Returns first successfully validated rule
* slime_misc:all/1 - expect list of rules. All validations must success. Returns last validation rule result
* slime_misc:sub/1 - sub map validation. Expect argument of type rules() to validate sub map
* slime_misc:compare/1 - expect compare rule of type compare() as argument.
* slime_misc:length/1 - check list(), binary() length. erlang:bit_size/1 for binary(), erlang:length/1 for list
* slime_misc:string_length/1 - check string length. UTF8 strings handled correctly