%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Convert dates.
%%%
%%% @version 0.1

-module(dates).
-author("eloipoch").


%% API
-export([date_parts/1]).


%% @doc Returns an ISO date format ("yyyy-mm-dd") as a list of
%% integers in the form [yyyy, mm, dd].
-spec(date_parts(string()) -> list()).

date_parts(DateISO) ->
  [Year, Month, Day] = re:split(DateISO, "[-]", [{return, list}]),
  [to_integer(Year), to_integer(Month), to_integer(Day)].



%% Internal functions


%% @doc Convert a string to integer.
-spec(to_integer(string()) -> integer()).

to_integer(String) ->
  element(1, string:to_integer(String)).
