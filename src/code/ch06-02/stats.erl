%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Stats module.
%%%
%%% @version 0.2

-module(stats).
-author("eloipoch").


%% API
-export([minimum/1, maximum/1, range/1]).


%% @doc Find the smallest value in a list.
-spec(minimum(list(number())) -> number()).

minimum(NumberList) ->
  [Head | RestOfNumbers] = NumberList,
  minimum(RestOfNumbers, Head).


%% @doc Find the greatest value in a list.
-spec(maximum(list(number())) -> number()).

maximum(NumberList) ->
  [Head | RestOfNumbers] = NumberList,
  maximum(RestOfNumbers, Head).


%% @doc Find the range in a list.
-spec(range(list(number())) -> list(number())).

range(NumberList) ->
  [minimum(NumberList), maximum(NumberList)].



%% Internal functions


%% @doc Returns the smallest value.
-spec(minimum(list(number()), number()) -> number()).

minimum([], SmallestValue) ->
  SmallestValue;

minimum([Value | Tail], SmallestValue) when Value < SmallestValue ->
  minimum(Tail, Value);

minimum([Value | Tail], SmallestValue) when Value > SmallestValue ->
  minimum(Tail, SmallestValue).


%% @doc Returns the greatest value.
-spec(maximum(list(number()), number()) -> number()).

maximum([], GreatestValue) ->
  GreatestValue;

maximum([Value | Tail], GreatestValue) when Value > GreatestValue ->
  maximum(Tail, Value);

maximum([Value | Tail], GreatestValue) when Value < GreatestValue ->
  maximum(Tail, GreatestValue).
