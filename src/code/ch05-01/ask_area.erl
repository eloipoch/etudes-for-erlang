%%% @author Eloi Poch <eloi.poch@gmail.com>
%%% @doc Ask to a user via shell the options to calc an area of a shape.
%%%
%%% @version 0.1

-module(ask_area).
-author("eloipoch").


%% API
-export([area/0]).


%% @doc Show the area for an asked shape options
-spec(area() -> no_return()).

area() ->
  calc_shape_area(ask_options()).



%% Internal functions


%% @doc Calculate shape area
-spec(calc_shape_area({string()|{error, char()}, number()|error, number()|error}) -> no_return()).

calc_shape_area({{error, ShapeChar}, _DimensionOne, _DimensionTwo}) ->
  io:format("Unknown shape ~p.~n", [ShapeChar]);

calc_shape_area({_Area, DimensionOne, DimensionTwo}) when DimensionOne < 0; DimensionTwo < 0 ->
  io:format("Both numbers must be greater than or equal to zero.~n");

calc_shape_area({_Area, error, _DimensionTwo}) ->
  io:format("Error in first dimension.~n");

calc_shape_area({_Area, _DimensionOne, error}) ->
  io:format("Error in second dimension.~n");

calc_shape_area({Area, DimensionOne, DimensionTwo}) ->
  geom:area({Area, DimensionOne, DimensionTwo}).


%% @doc Ask the shape options
-spec(ask_options() -> no_return()).

ask_options() ->
  Shape = ask_shape(),
  {DimensionOne, DimensionTwo} = ask_shape_dimensions(Shape),
  {Shape, DimensionOne, DimensionTwo}.


%% @doc Ask the shape type
-spec(ask_shape() -> atom()).

ask_shape() ->
  ShapeChar = [hd(io:get_line("R)ectangle, T)riangle, or E)llipse ? "))],
  covert_shape_char_to_shape_atom(ShapeChar).


%% @doc Ask the shape dimensions
-spec(ask_shape_dimensions(atom()) -> {number(), number()}|{error|error}).

ask_shape_dimensions(rectangle) -> {ask_dimension("width"), ask_dimension("height")};
ask_shape_dimensions(triangle)  -> {ask_dimension("base"), ask_dimension("height")};
ask_shape_dimensions(ellipse)   -> {ask_dimension("major axis"), ask_dimension("minor axis")};
ask_shape_dimensions(_)         -> {error, error}.


%% @doc Convert a char in atom than describe the shape
-spec(covert_shape_char_to_shape_atom(char()) -> atom()|{error, char()}).

covert_shape_char_to_shape_atom(Char) when Char == "r"; Char == "R" -> rectangle;
covert_shape_char_to_shape_atom(Char) when Char == "t"; Char == "T" -> triangle;
covert_shape_char_to_shape_atom(Char) when Char == "e"; Char == "E" -> ellipse;
covert_shape_char_to_shape_atom(Char)                               -> {error, Char}.


%% @doc Ask a dimension of the shape
-spec(ask_dimension(string()) -> number()|error).

ask_dimension(Prompt) ->
  String = io:get_line("Enter " ++ Prompt ++ " > "),
  {FloatTest, _} = string:to_float(String),
  case FloatTest of
    error -> {Number, _} = string:to_integer(String);
    _ -> Number = FloatTest
  end,
  Number.
