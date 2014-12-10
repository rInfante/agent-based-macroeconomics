-module(item_selection).
%%public
-export(
		[
			choose_weighted_random_item/1,
			choose_random_item/1,
			filter_out_second_list_from_first_list/2, 
			replace_item_in_list/3
		]
		).
%%test
-export([
			only_quantities/1, cumulative_quantities/1, cumulative_ranges/1, indexed_items/1, 
			find_indexed_cumulative_range/2		
		]).

%ItemQuantityTupleList = list of tuples {item, N}. Example: [{a,1],{b,3},{c,2},{d,4},...]
%returns QuantityList. Example: [1,3,2,4,...]	
only_quantities(ItemQuantityTupleList) ->
	lists:map(fun({_,Quantity})->Quantity end, ItemQuantityTupleList).

%QuantityLiust = List of quantities. Example: [1,2,5,2,1]	
%returns cumulative quantities: [1,3,8,10,11]
cumulative_quantities(QuantityList) ->
	{CumulativeQuantities, TotalQuantity} = lists:mapfoldl(fun(X, Sum) -> {X+Sum, X+Sum} end, 0, QuantityList),
	{CumulativeQuantities, TotalQuantity}.
	
%CumulativeQuantities is a list of cumulative quantities like: [1,3,8,10,11]
%CumulativeRanges returns a list of tuples like: [{0,1},{2, 3},{4, 8}, {9, 10},{11,11}]	
cumulative_ranges(CumulativeQuantities) ->
	ShiftedCumulativeQuantities = lists:map(fun(X)->X+1 end, CumulativeQuantities),
	ZeroBasedShiftedCumulativeQuantities = lists:append([0],lists:droplast(ShiftedCumulativeQuantities)),
	lists:zip(ZeroBasedShiftedCumulativeQuantities, CumulativeQuantities).
	
%returns a list of 2-tuple (pair) of {index, item}. Example: from [a,b,c,d] we get: [{0,a},{1,b},{2,c},{3,d}].
indexed_items(ItemList) ->
	IndexList = lists:seq(1,length(ItemList)),
	lists:zip(IndexList, ItemList).

% this function returns the range which includes to the value to find	
%IndexedCumulativeRanges is a liste of tuples like: [{1,{0,1}},{2,{2, 6}},{3,{7, 9}}, {4, {10, 12}},{5,{13,13}}]
% 	where a single tuple is: {Index, {StartRange, EndRange}}
%Value is a numeric value between 0 and the last value of the cumulative range
find_indexed_cumulative_range(IndexedCumulativeRanges, Value) ->
	lists:filter(fun({_Index, {StartRange, EndRange}}) -> (StartRange =< Value) and (Value =< EndRange) end, IndexedCumulativeRanges).
		
choose_weighted_random_item(ItemQuantityTupleList) ->
	OnlyQuantities = only_quantities(ItemQuantityTupleList),
	{CumulativeQuantities, TotalQuantity} = cumulative_quantities(OnlyQuantities),
	CumulativeRanges = cumulative_ranges(CumulativeQuantities),
	IndexedCumulativeRanges = indexed_items(CumulativeRanges),
	RandomChoice = random:uniform(TotalQuantity),
	[{Index, {_,_}}] = find_indexed_cumulative_range(IndexedCumulativeRanges,RandomChoice),
	{Item, _} = lists:nth(Index, ItemQuantityTupleList),
	Item.
	
choose_random_item(ItemList) ->
	ItemQuantityTupleList = lists:map(fun(Item) -> {Item, 1} end, ItemList),
	Item = choose_weighted_random_item(ItemQuantityTupleList),
	Item.

%TODO:probably this function is redundant because already provided by lists library	
filter_out_second_list_from_first_list(FirstList, SecondList) ->
	lists:subtract(FirstList, SecondList).
	
replace_item_in_list(List, CurrentItem, NewItem) ->
	lists:map(fun(X)->
					if
						X == CurrentItem ->
							NewItem;
						true ->
							X
					end
					end, List).