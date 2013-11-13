%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Functions for raising a number to an integer power
%%% and finding the Nth root of a number using Newton's method.
%%%
%%% @version 0.1

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
  Base * raise(Base, Exponential - 1);

raise(Base, Exponential) when Exponential < 0 ->
  1.0 / raise(Base, - Exponential).
