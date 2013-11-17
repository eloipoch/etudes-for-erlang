%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Stats module.
%%%
%%% @version 0.1

-module(stats).
-author("eloipoch").


%% API
-export([minimum/1]).


%% @doc Find the smallest value in a list.
-spec(minimum(list(number())) -> number()).

minimum(NumberList) ->
  [Head | RestOfNumbers] = NumberList,
  minimum(RestOfNumbers, Head).



%% Internal functions


%% @doc Returns the smallest value.
-spec(minimum(list(number()), number()) -> number()).

minimum([], SmallestValue) ->
  SmallestValue;

minimum([Value | Tail], SmallestValue) when Value < SmallestValue ->
  minimum(Tail, Value);

minimum([Value | Tail], SmallestValue) when Value > SmallestValue ->
  minimum(Tail, SmallestValue).
