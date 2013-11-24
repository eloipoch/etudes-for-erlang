%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Generate a random set of teeth.
%%%
%%% @version 0.1

-module(non_fp).
-author("eloipoch").


%% API
-export([generate_teeth/2, generate_tooth/1, test_teeth/0]).


%% @doc Generate a list of lists, six numbers per tooth, giving random
%% pocket depths. Takes a string where T="there's a tooth there"
%% and F="no tooth"), and a float giving probability that a tooth is good.
-spec(generate_teeth(string(), float()) -> list(list(integer()))).

generate_teeth(TeethPresent, ProbabilityToBeGood) ->
  random:seed(now()),
  generate_teeth(TeethPresent, ProbabilityToBeGood, []).


%% @doc Generates a list of six numbers for a single tooth. Choose a
%% random number between 0 and 1. If that number is less than the probability
%% of a good tooth, it sets the "base depth" to 2, otherwise it sets the base
%% depth to 3.
-spec(generate_tooth(float()) -> list(integer())).

generate_tooth(ProbabilityToBeGood) ->
  IsAGoodTooth = random:uniform() < ProbabilityToBeGood,
  generate_tooth(IsAGoodTooth, 6, []).


%% @doc Print a test teeth.
-spec(test_teeth() -> no_return()).

test_teeth() ->
  Teeth= "FTTTTTTTTTTTTTTFTTTTTTTTTTTTTTTT",
  print_tooth(generate_teeth(Teeth, 0.75)).



%% Internal functions


%% @doc Helper function that adds tooth data to the ultimate result.
-spec(generate_teeth(string(), float(), list(list(integer()))) -> list(list(integer()))).

generate_teeth([], _ProbabilityToBeGood, TeethResult) ->
  lists:reverse(TeethResult);

generate_teeth([$F | TeethPresent], _ProbabilityToBeGood, Result) ->
  generate_teeth(TeethPresent, _ProbabilityToBeGood, [[0] | Result]);

generate_teeth([$T | TeethPresent], ProbabilityToBeGood, Result) ->
  generate_teeth(TeethPresent, ProbabilityToBeGood, [generate_tooth(ProbabilityToBeGood) | Result]).


%% @doc Helper function that generate a tooth.
-spec(generate_tooth(string(), float(), list(list(integer()))) -> list(list(integer()))).

generate_tooth(true, TeethToGenerate, GeneratedTeeth) ->
  generate_tooth(2, TeethToGenerate, GeneratedTeeth);

generate_tooth(false, TeethToGenerate, GeneratedTeeth) ->
  generate_tooth(3, TeethToGenerate, GeneratedTeeth);

generate_tooth(_BaseDepth, 0, GeneratedTeeth) ->
  GeneratedTeeth;

generate_tooth(BaseDepth, TeethToGenerate, GeneratedTeeth) ->
  [BaseDepth + random:uniform(3) - 2 | generate_tooth(BaseDepth, TeethToGenerate - 1, GeneratedTeeth)].


%% @doc Helper function that prints teeth.
-spec(print_tooth(list(list(integer()))) -> no_return()).

print_tooth([]) ->
  io:format("Finished.~n");

print_tooth([H|T]) ->
  io:format("~p~n", [H]),
  print_tooth(T).