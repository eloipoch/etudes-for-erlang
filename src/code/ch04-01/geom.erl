%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Functions for calculating areas of geometric shapes.
%%%
%%% @version 0.3

-module(geom).
-export([area/3]).


%% @doc Calculates the area of a shape (rectangle, triangle
%% and ellipse)
-spec(area(atom(), number(), number()) -> number()).

area(Shape, DimensionOne, DimensionTwo) when DimensionOne >= 0, DimensionTwo >= 0 ->
  case Shape of
    rectangle -> DimensionOne * DimensionTwo;
    triangle  -> DimensionOne * DimensionTwo / 2.0;
    ellipse   -> math:pi() * DimensionOne * DimensionTwo
  end.
