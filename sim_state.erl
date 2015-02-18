-module(sim_state).

-export([get_value/2]).

-include_lib("record_defs.hrl").

get_value(days_in_one_month, SimState) ->
	SimState#sim_state.days_in_one_month;
get_value(num_household_to_firm_trading_relations, SimState) ->
	SimState#sim_state.num_household_to_firm_trading_relations;
get_value(num_consecutive_months_with_all_positions_filled_upper_limit, SimState) ->
	SimState#sim_state.num_consecutive_months_with_all_positions_filled_upper_limit;
get_value(wage_growth_rate_uniform_distribution_upper_support, SimState) ->
	SimState#sim_state.wage_growth_rate_uniform_distribution_upper_support;
get_value(inventory_upper_limit_ratio, SimState) ->
	SimState#sim_state.inventory_upper_limit_ratio;
get_value(inventory_lower_limit_ratio, SimState) ->
	SimState#sim_state.inventory_lower_limit_ratio;
get_value(price_upper_limit_ratio, SimState) ->
	SimState#sim_state.price_upper_limit_ratio;
get_value(price_lower_limit_ratio, SimState) ->
	SimState#sim_state.price_lower_limit_ratio;	
get_value(probability_of_setting_new_price, SimState) ->
	SimState#sim_state.probability_of_setting_new_price;
get_value(price_growth_rate_uniform_distribution_upper_support, SimState) ->
	SimState#sim_state.price_growth_rate_uniform_distribution_upper_support;
get_value(probability_of_household_picking_new_provider_firm, SimState) ->
	SimState#sim_state.probability_of_household_picking_new_provider_firm;
get_value(price_threshold_of_household_picking_new_provider_firm, SimState) ->
	SimState#sim_state.price_threshold_of_household_picking_new_provider_firm;
get_value(max_number_potential_employers_visited, SimState) ->
	SimState#sim_state.max_number_potential_employers_visited;
get_value(probability_of_household_visiting_potential_new_employer, SimState) ->
	SimState#sim_state.probability_of_household_visiting_potential_new_employer;
get_value(planned_consumption_increase_decaying_rate, SimState) ->
	SimState#sim_state.planned_consumption_increase_decaying_rate;
get_value(max_number_provider_firms_visited, SimState) ->
	SimState#sim_state.max_number_provider_firms_visited;
get_value(technology_productivity_parameter, SimState) ->
	SimState#sim_state.technology_productivity_parameter;
get_value(claimed_wage_rate_percentage_reduction_if_unemployed, SimState) ->
	SimState#sim_state.claimed_wage_rate_percentage_reduction_if_unemployed;
get_value(firm_employees_lookup, SimState) ->
	SimState#sim_state.firm_employees_lookup;
get_value(household_ids, SimState) ->
	SimState#sim_state.household_ids;
get_value(firm_ids, SimState) ->
	FirmToEmployeesLookup = SimState#sim_state.firm_employees_lookup,	
	lists:map(fun({FirmId, _EmployeeIds}) ->FirmId end, FirmToEmployeesLookup).

%%TODO: REMOVE	
%%FirmToEmployeesLookup = [{1, [11,12]}, {2, [21, 22, 23]}, {3, [31, 32]} , ...
%%return id list: [1, 2, 3, 4, ...
%%get_firm_ids_from_lookup(FirmToEmployeesLookup) ->
%%	lists:map(fun({FirmId, _EmployeeIds}) ->FirmId end, FirmToEmployeesLookup).	