-module(numerics).
-export([uniform/1, is_happening_with_probability/1, percent_difference/2]).

%Delta is between 0.0 and 1.0
uniform(Delta) -> 
	random:uniform() * Delta.

%Probability is between 0.0 and 1.0
is_happening_with_probability(Probability) ->
	Choice = random:uniform(100),
	if 
		Choice < Probability*100.0 ->
			true;
		true ->
			false
	end.

%CHECK THIS!
%Number1 and Number2 are between 0.0 and 1.0
percent_difference(Number1, Number2) ->
       1.0 - Number1 / Number2.