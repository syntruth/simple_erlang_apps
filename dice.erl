-module(dice).
-export([roll/2, roll/3]).
-export([display_tally/1]).
-export([d4/0, d4/1, d4/2]). 
-export([d6/0, d6/1, d6/2]).
-export([d8/0, d8/1, d8/2]).
-export([d10/0, d10/1, d10/2]).
-export([d12/0, d12/1, d12/2]).
-export([d20/0, d20/1, d20/2]).
-export([d100/0, d100/1, d100/2]).

seed() ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3).


roll(Num, Sides) ->
  seed(),
  Tally = get_tally(Num, Sides),
  {lists:sum(Tally), lists:reverse(lists:sort(Tally))}.

roll(Num, Sides, Mod) ->
  {Total, Tally} = roll(Num, Sides),
  {Total + Mod, Tally}.


get_tally(0, _) -> [];
get_tally(Num, Sides) -> 
  [random:uniform(Sides) | get_tally(Num - 1, Sides)].


d4() -> d4(0).
d4(Mod) -> roll(1, 4, Mod).
d4(Num, Mod) -> roll(Num, 4, Mod).


d6() -> d6(0).
d6(Mod) -> roll(1, 6, Mod).
d6(Num, Mod) -> roll(Num, 6, Mod).


d8() -> d8(0).
d8(Mod) -> roll(1, 8, Mod).
d8(Num, Mod) -> roll(Num, 8, Mod).


d10() -> d10(0).
d10(Mod) -> roll(1, 10, Mod).
d10(Num, Mod) -> roll(Num, 10, Mod).


d12() -> d12(0).
d12(Mod) -> roll(1, 12, Mod).
d12(Num, Mod) -> roll(Num, 12, Mod).


d20() -> d20(0).
d20(Mod) -> roll(1, 20, Mod).
d20(Num, Mod) -> roll(Num, 20, Mod).


d100() -> d100(0).
d100(Mod) -> roll(1, 100, Mod).
d100(Num, Mod) -> roll(Num, 100, Mod).


display_tally([H|T]) ->
  TStr = "[" ++ integer_to_list(H) ++ "]",
  display_tally(TStr, T).

display_tally(TStr, []) -> TStr;
display_tally(TStr, [H|T]) ->
  display_tally(TStr ++ "[" ++ integer_to_list(H) ++ "]", T).

