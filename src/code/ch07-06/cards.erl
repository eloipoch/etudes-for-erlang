%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Functions for playing a card game.
%%%
%%% @version 0.2

-module(cards).
-author("eloipoch").


%% API
-export([make_deck/0, show_deck/1, shuffle/1]).


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


%% @doc Shuffle a deck.
-spec(shuffle(list({string(), string()})) -> list({string(), string()})).

shuffle(Deck) ->
  shuffle(Deck, []).



%% Internal functions


%% Helper function to shuffle a deck. Split the deck randomly, pick the first card into a
%% new shuffled deck and repeat until the last card.
-spec(shuffle(list({string(), string()}), list({string(), string()})) -> list({string(), string()})).

shuffle([], ShuffledDeck) ->
  ShuffledDeck;

shuffle(Deck, ShuffledDeck) ->
  {LeftSplitDeck, [PickedCard | RightSplitDeck]} = lists:split(random:uniform(length(Deck)) - 1, Deck),
  shuffle(LeftSplitDeck ++ RightSplitDeck, [PickedCard | ShuffledDeck]).