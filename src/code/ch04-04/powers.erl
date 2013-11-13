%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Functions for raising a number to an integer power.
%%%
%%% @version 0.2

-module(powers).
-author("eloipoch").


%% API
-export([raise/2]).


%% @doc Raise a number <code>Base</code> to an integer power
%% <code>Exponential</code>.
-spec(raise(number(), integer()) -> number()).

raise(_Base, 0) ->
  1;

raise(Base, 1) ->
  Base;

raise(Base, Exponential) when Exponential > 0 ->
  raise(Base, Exponential, 1);

raise(Base, Exponential) when Exponential < 0 ->
  1.0 / raise(Base, - Exponential).



%% @doc Helper function to raise <code>Base</code> to
%% <code>Exponential</code> by passing an <code>Accumulator</code>
%% from call to call.
-spec(raise(number(), integer(), integer()) -> number()).

raise(_Base, 0, Accumulator) ->
  Accumulator;

raise(Base, Exponential, Accumulator) ->
  raise(Base, Exponential - 1, Base * Accumulator).
