%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Create a deck of cards.
%%%
%%% @version 0.1

-module(cards).
-author("eloipoch").


%% API
-export([make_deck/0, show_deck/1]).


%% @doc Create a deck.
-spec(make_deck() -> list({string(), string()})).

make_deck() ->
  ValueList = ["A", 2, 3, 4, 5, 6, 7, 8, 9, "J", "Q", "K"],
  SuitList  = ["Clubs", "Diamonds", "Hearts", "Spades"],
 [{Value, Suit} || Value <- ValueList, Suit <- SuitList].


%% @doc Show a deck.
-spec(show_deck(list({string(), string()})) -> no_return()).

show_deck(Deck) ->
  lists:foreach(fun(Item) -> io:format("~p~n", [Item]) end, Deck).