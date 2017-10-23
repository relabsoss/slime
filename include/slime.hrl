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
