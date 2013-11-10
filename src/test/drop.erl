-module(drop).

-export([fall_velocity/1]).

-spec(fall_velocity(number()) -> number()).
fall_velocity(Distance) ->
  math:sqrt(2 * 9.8 * Distance).
