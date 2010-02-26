-module(test).
-export([concat/1]).
-export([flatten/1]).
-export([quicksort/1]).

concat(L) -> concat_sub(L, []).

concat_sub(L, Acc) when not is_list(L) ->
  Acc ++ [L];
concat_sub([], Acc) -> Acc;
concat_sub([H|T], Acc) ->
  Acc2 = concat_sub(H, []),
  concat_sub(T, Acc ++ Acc2).


flatten([]) -> [];
flatten([H|T]) -> concat(H) ++ concat(T).


quicksort([]) -> [];
quicksort([H|T]) ->
  quicksort([N || N <- T, T =< H]) ++ [H] ++ quicksort([M || M <- T, T > H]).


