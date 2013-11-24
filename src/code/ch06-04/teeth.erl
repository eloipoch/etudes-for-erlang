%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Show teeth that need attention due to excessive pocket depth..
%%%
%%% @version 0.1

-module(teeth).
-author("eloipoch").


%% API
-export([alert/1]).


%% @doc Returns the list of the bad teeth.
-spec(alert(list(list(integer()))) -> list(integer())).

alert(ToothList) ->
  alert(ToothList, 1, []).



%% Internal functions


%% @doc Helper function that returns the bad teeth.
-spec(alert(list(list(integer())), integer(), list(integer())) -> list(integer())).

alert([], _ToothPosition, BadTeeth) ->
  lists:reverse(BadTeeth);

alert([ToothLocations | ToothList], ToothPosition, BadTeeth) ->
  NewBadTeeth = add_tooth_to_alert_list(is_bad_tooth(ToothLocations), ToothPosition, BadTeeth),
  alert(ToothList, ToothPosition + 1, NewBadTeeth).


%% @doc Helper function that returns if a tooth is bad.
-spec(is_bad_tooth(list(integer())) -> boolean()).

is_bad_tooth(Tooth) ->
  stats:maximum(Tooth) >= 4.


%% @doc Helper function that add a bad tooth to the bad teeth.
-spec(add_tooth_to_alert_list(boolean(), integer(), list(integer())) -> list(integer())).

add_tooth_to_alert_list(true, ToothPosition, BadTeeth) ->
  [ToothPosition | BadTeeth];

add_tooth_to_alert_list(false, _ToothPosition, BadTeeth) ->
  BadTeeth.
