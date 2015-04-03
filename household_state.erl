-module(household_state).

-export([get_value/2, get_values/2, household_id_to_str/1, household_id_to_atom/1, get_all_fields/0, get_all_values/1]).

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
get_value(dash_delimited_provider_firms_ids, HouseholdState) ->
	ProviderFirmsIds = HouseholdState#household_state.provider_firms_ids,
	item_selection:dash_delimited_integer_list(ProviderFirmsIds);	
get_value(employer_firm_id, HouseholdState) ->
	HouseholdState#household_state.employer_firm_id;
	
get_value(household_id_as_atom, HouseholdState) ->
	FirmId = HouseholdState#household_state.household_id,	
	household_id_to_atom(FirmId).	
	
get_all_fields()->
	[household_id, reservation_wage_rate_h, liquidity_h, planned_monthly_consumption_expenditure, dash_delimited_provider_firms_ids, employer_firm_id].	
	
get_values(Args, HouseholdState) ->
	[get_value(Arg, HouseholdState) || Arg <- Args].
	
get_all_values(HouseholdState)->
	get_values(get_all_fields(), HouseholdState).	

household_id_to_str(HouseholdId) ->
	string:concat("HH", integer_to_list(HouseholdId)).
	
household_id_to_atom(HouseholdId) ->
	HouseholdIdStr = household_id_to_str(HouseholdId),
	list_to_atom(HouseholdIdStr).