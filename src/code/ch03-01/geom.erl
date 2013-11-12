%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Functions for calculating areas of geometric shapes.
%%%
%%% @version 0.2

-module(geom).
-export([area/3]).


%% @doc Calculates the area of a shape (rectangle, triangle
%% and ellipse)
-spec(area(atom(), number(), number()) -> number()).

area(rectangle, Lenght, Width) ->
  Lenght * Width;
area(triangle, Base, Height) ->
  Base * Height / 2.0;
area(ellipse, MajorRadius, MinorRadius) ->
  math:pi() * MajorRadius * MinorRadius.
