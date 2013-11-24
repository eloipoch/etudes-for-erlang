%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Stats module.
%%%
%%% @version 0.4

-module(stats).
-author("eloipoch").


%% API
-export([minimum/1, maximum/1, range/1, mean/1, standard_deviation/1]).


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


%% @doc Find the mean of a list of numbers.
-spec(mean(list(number())) -> number()).

mean(NumberList) ->
  lists:foldl(fun(X, Sum) -> X + Sum end, 0, NumberList) / length(NumberList).


%% @doc Find the standard deviation of a list of numbers.
-spec(standard_deviation(list(number())) -> number()).

standard_deviation(NumberList) ->
  Quantity = length(NumberList),
  {SumList, SumSquareList} = lists:foldl(fun standard_deviation_sums/2, {0, 0}, NumberList),
  math:sqrt((Quantity * SumSquareList - SumList * SumList) / (Quantity * (Quantity - 1))).



%% Internal functions


%% @doc Returns the smallest value.
-spec(minimum(list(number()), number()) -> number()).

minimum([], SmallestValue) ->
  SmallestValue;

minimum([Value | Tail], SmallestValue) when Value =< SmallestValue ->
  minimum(Tail, Value);

minimum([Value | Tail], SmallestValue) when Value > SmallestValue ->
  minimum(Tail, SmallestValue).


%% @doc Returns the greatest value.
-spec(maximum(list(number()), number()) -> number()).

maximum([], GreatestValue) ->
  GreatestValue;

maximum([Value | Tail], GreatestValue) when Value >= GreatestValue ->
  maximum(Tail, Value);

maximum([Value | Tail], GreatestValue) when Value < GreatestValue ->
  maximum(Tail, GreatestValue).


%% @doc Returns the sum and the sum of squares of the given value.
-spec(standard_deviation_sums(number(), {number(), number()}) -> {number(), number()}).

standard_deviation_sums(Value, {Sum, SumSquare}) ->
  {Value + Sum, (Value * Value) + SumSquare}.
