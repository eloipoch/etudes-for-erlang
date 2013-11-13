%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Finding the greatest common divisor (GCD) of two integers
%%% following the method devised by Edsger W Dijkstra
%%%
%%% @version 0.1

-module(dijkstra).
-author("Eloi Poch").

%% API
-export([gcd/2]).


%% @doc Calculates the  greatest common divisor (GCD) of two integers
-spec(gcd(integer(), integer()) -> integer()).


gcd(NumberOne, NumberTwo) when NumberOne > NumberTwo ->
  gcd(NumberOne - NumberTwo, NumberTwo);
gcd(NumberOne, NumberTwo) when NumberOne < NumberTwo ->
  gcd(NumberOne, NumberTwo - NumberOne);
gcd(NumberOne, _NumberTwo) ->
  NumberOne.
