-module(numerics).
-export([uniform/1, is_happening_with_probability/1, 
	increase/2, decrease/2, increase_with_probability/3, decrease_with_probability/3,
	percent_difference/2, list_average/1]).

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
	
increase(Value, Factor) ->
	Value * (1.0 + Factor).
decrease(Value, Factor) ->
	Value * (1.0 - Factor).	

increase_with_probability(Probability, Value, Factor) ->
	case is_happening_with_probability(Probability) of
		true ->  increase(Value, Factor);
		false -> Value
	end.
decrease_with_probability(Probability, Value, Factor) ->
	case is_happening_with_probability(Probability) of
		true ->  decrease(Value, Factor);
		false -> Value
	end.	
	
%CHECK THIS!
%Number1 and Number2 are between 0.0 and 1.0
percent_difference(Number1, Number2) ->
       1.0 - Number1 / Number2.
	   
list_average(List) ->
	lists:sum(List) / length(List).