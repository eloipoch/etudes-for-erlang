%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Calculate derivative of functions.
%%%
%%% @version 0.1

-module(calculus).
-author("eloipoch").

-define(DELTA, 1.0e-10).

%% API
-export([derivative/2]).


%% @doc Calculates the derivative of a function in a point.
-spec(derivative(function(), float()) -> float()).

derivative(Function, Point) ->
  (Function(Point + ?DELTA) - Function(Point)) / ?DELTA.