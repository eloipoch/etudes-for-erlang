%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Convert dates.
%%%
%%% @version 0.2

-module(dates).
-author("eloipoch").


%% API
-export([date_parts/1, julian/1]).


%% @doc Returns an ISO date format ("yyyy-mm-dd") as a list of
%% integers in the form [yyyy, mm, dd].
-spec(date_parts(string()) -> list(integer())).

date_parts(DateISO) ->
  [Year, Month, Day] = re:split(DateISO, "[-]", [{return, list}]),
  [to_integer(Year), to_integer(Month), to_integer(Day)].


%% @doc Returns an ISO date format ("yyyy-mm-dd") as a julian date.
-spec(julian(string()) -> list()).

julian(DateISO) ->
  [Year, Month, Day] = date_parts(DateISO),
  julian({Year, Month, Day}, getDaysPerMonth(is_leap_year(Year)), 0).



%% Internal functions


%% @doc Convert a string to integer.
-spec(to_integer(string()) -> integer()).

to_integer(String) ->
  element(1, string:to_integer(String)).


%% @doc Return a list with the quantity of days per month.
-spec(getDaysPerMonth(integer()) -> list(integer())).

getDaysPerMonth(IsLeapYear) when IsLeapYear == true ->
  [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

getDaysPerMonth(IsLeapYear) when IsLeapYear == false ->
  [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31].


%% @doc Returns the quantity of days since 1 from January.
-spec(julian({integer(), integer(), integer()}, list(), integer()) -> integer()).

julian({_Year, 0, Day}, _DaysPerMonth, Total) ->
  Total + Day;

julian({Year, Month, Day}, DaysPerMonth, Total) ->
  [DaysPerActualMonth | RestOfDaysPerMonth] = DaysPerMonth,
  julian({Year, Month - 1, Day}, RestOfDaysPerMonth, DaysPerActualMonth + Total).


%% @doc Returns if a year is a leap year or not.
-spec(is_leap_year(integer()) -> boolean()).

is_leap_year(Year) ->
  (Year rem 4 == 0 andalso Year rem 100 /= 0)
    orelse (Year rem 400 == 0).
