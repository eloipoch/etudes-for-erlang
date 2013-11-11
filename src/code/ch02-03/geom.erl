%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Functions for calculating areas of geometric shapes.
%%%
%%% @version 0.1

-module(geom).
-export([area/2]).


%% @doc Calculates the area of a rectangle, given the length and width.
%% Returns the product of its arguments
-spec(area(number(),number()) -> number()).
area(Lenght, Width) ->
  Lenght * Width.
