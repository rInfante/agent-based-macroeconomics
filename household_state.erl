-module(household_state).

-export([get_value/2, get_values/2, household_id_to_str/1, household_id_to_atom/1]).

-include_lib("record_defs.hrl").

get_value(household_id, HouseholdState) ->
	HouseholdState#household_state.household_id;
get_value(reservation_wage_rate_h, HouseholdState) ->
	HouseholdState#household_state.reservation_wage_rate_h;
get_value(liquidity_h, HouseholdState) ->
	HouseholdState#household_state.liquidity_h;
get_value(planned_monthly_consumption_expenditure, HouseholdState) ->
	HouseholdState#household_state.planned_monthly_consumption_expenditure;
get_value(provider_firms_ids, HouseholdState) ->
	HouseholdState#household_state.provider_firms_ids;	
get_value(employer_firm_id, HouseholdState) ->
	HouseholdState#household_state.employer_firm_id;
get_value(household_id_as_atom, HouseholdState) ->
	FirmId = HouseholdState#household_state.household_id,	
	household_id_to_atom(FirmId).	
	
get_values(Args, HouseholdState) ->
	[get_value(Arg, HouseholdState) || Arg <- Args].

household_id_to_str(HouseholdId) ->
	string:concat("HH", integer_to_list(HouseholdId)).
	
household_id_to_atom(HouseholdId) ->
	HouseholdIdStr = household_id_to_str(HouseholdId),
	list_to_atom(HouseholdIdStr).