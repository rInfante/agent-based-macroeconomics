-module(household_state).

-export([get_value/2]).

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
	HouseholdState#household_state.employer_firm_id.