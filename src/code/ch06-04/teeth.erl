%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Convert dates.
%%%
%%% @version 0.2

-module(teeth).
-author("eloipoch").


%% API
-export([alert/1]).


%% @doc Returns the list of the teeth with alerts.
-spec(alert(list(list(integer()))) -> list(integer())).

alert(ToothList) ->
  alert(ToothList, 1, []).



%% Internal functions


%% @doc Returns the list of the teeth with alerts.
-spec(alert(list(list(integer())), integer(), list(integer())) -> list(integer())).

alert([], _ToothNumber, AlertList) ->
  lists:reverse(AlertList);

alert([ToothLocations | ToothList], ToothNumber, AlertList) ->
  case has_alerts(ToothLocations) of
    true -> alert(ToothList, ToothNumber + 1, [ToothNumber | AlertList]);
    false -> alert(ToothList, ToothNumber + 1, AlertList)
  end.


%% @doc Returns if the tooth has an alert.
-spec(has_alerts(list(integer())) -> boolean()).

has_alerts(ToothLocations) ->
  stats:maximum(ToothLocations) >= 4.
