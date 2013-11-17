%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Functions for calculating areas of geometric shapes.
%%%
%%% @version 0.4

-module(geom).
-author("eloipoch").


%% API
-export([area/1]).


%% @doc Calculates the area of a shape (rectangle, triangle
%% and ellipse) and return value 0 for unknown
-spec(area({atom(), number(), number()}) -> number()).

area({rectangle, Length, Width}) when Length >= 0, Width >= 0 ->
  Length * Width;

area({triangle, Base, Height}) when Base >= 0, Height >= 0 ->
  Base * Height / 2.0;

area({ellipse, MajorRadius, MinorRadius}) when MajorRadius >= 0, MinorRadius >= 0 ->
  math:pi() * MajorRadius * MinorRadius;

area(_) ->
  0.

