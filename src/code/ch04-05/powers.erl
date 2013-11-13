%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Functions for raising a number to an integer power.
%%%
%%% @version 0.3

-module(powers).
-author("eloipoch").


%% API
-export([raise/2, nth_root/2]).


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


%% @doc Finds the nth root of a number <code>Base</code>
%% where <code>Root</code> is an integer.
-spec(nth_root(number(), integer()) -> number()).

nth_root(Base, Root) ->
  Approximation = Base / 2.0,
  nth_root(Base, Root, Approximation).



%% @doc Helper function to raise <code>Base</code> to
%% <code>Exponential</code> by passing an <code>Accumulator</code>
%% from call to call.
-spec(raise(number(), integer(), integer()) -> number()).

raise(_Base, 0, Accumulator) ->
  Accumulator;

raise(Base, Exponential, Accumulator) ->
  raise(Base, Exponential - 1, Base * Accumulator).


%% @doc Helper function to finds the nth root of a number
%% <code>Base</code> where <code>Root</code> is an integer.
-spec(nth_root(number(), integer(), number()) -> number()).

nth_root(Base, Root, Approximation) ->
  io:format("Current guess is ~p~n", [Approximation]),
  F                 = raise(Approximation, Root) - Base,
  FPrime            = Root * raise(Approximation, Root - 1),
  NextApproximation = Approximation - F / FPrime,
  Change            = Approximation - NextApproximation,
  if
    Change < 1.0e-8 ->
      NextApproximation;
    true ->
      nth_root(Base, Root, NextApproximation)
  end.