%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Use pattern matching in a list comprehension..
%%%
%%% @version 0.1

-module(patmatch).
-author("eloipoch").


%% API
-export([older_males/0, older_or_male/0]).


%% @doc Returns the list of people that are a male over 40.
-spec(older_males() -> list(string())).

older_males() ->
  [Name || {Name, Gender, Age} <- get_people(), Gender == $M, Age > 40].


%% @doc Returns the list of people that are males or over 40.
-spec(older_or_male() -> list(string())).

older_or_male() ->
  [Name || {Name, Gender, Age} <- get_people(), (Gender == $M) orelse (Age > 40)].



%% Internal function


%% @doc Helper function that creates a list of persons
-spec(get_people() -> list({string(), char(), integer()})).

get_people() ->
  [
    {"Federico", $M, 22},
    {"Kim", $F, 45},
    {"Hansa", $F, 30},
    {"Vu", $M, 47},
    {"Cathy", $F, 32},
    {"Elias", $M, 50}
  ].