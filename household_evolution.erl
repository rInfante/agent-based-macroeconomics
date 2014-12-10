-module(household_evolution).

-export(
		[
			select_simulation_firm_agent_ids/1,
			choose_potential_employer_firm/1,
			choose_random_provider_firm_id/1,
			select_unconnected_firm_ids/1
		]
 ).
 
 %TODO:move to include file to share this definition across several modules	
-record(sim_config, 
	{
		days_in_one_month,
		num_household_to_firm_trading_relations,
		num_consecutive_months_with_all_positions_filled_upper_limit,%gamma
		wage_growth_rate_uniform_distribution_upper_support,%delta
		inventory_upper_limit_ratio,%uphi_upper
		inventory_lower_limit_ratio,%uphi_lower
		price_upper_limit_ratio,% lphi_upper
		price_lower_limit_ratio,%lphi_lower
		probability_of_setting_new_price,%theta
		price_growth_rate_uniform_distribution_upper_support,%upsilon
		probability_of_household_picking_new_provider_firm,
        price_threshold_of_household_picking_new_provider_firm,
		max_number_potential_employers_visited,
		probability_of_household_visiting_potential_new_employer,
		planned_consumption_increase_decaying_rate,
		max_number_provider_firms_visited,
		technology_productivity_parameter,%lambda
		claimed_wage_rate_percentage_reduction_if_unemployed,
		
		firm_ids,
		household_ids
    }).	
 
 %TODO move this to shared HRC file
 -record(household_state, 
	{
		instance_name, 
		reservation_wage_rate_h, %w_h
		liquidity_h, %m_h
		planned_monthly_consumption_expenditure, %c_r_h
		provider_firms_ids, %type A firms
		employer_firm_id, % type B firm
		days_in_one_month,
		
		sim_configuration
	}).

select_simulation_firm_agent_ids(#household_state{sim_configuration=SimConfirguration}) ->
    SimConfirguration#sim_config.firm_ids.

choose_potential_employer_firm(HouseholdState) ->
    FirmIds = select_simulation_firm_agent_ids(HouseholdState),
    item_selection:choose_random_item(FirmIds).
	
choose_random_provider_firm_id(#household_state{provider_firms_ids=ProviderFirmIds}) ->
    item_selection:choose_random_item(ProviderFirmIds).

select_unconnected_firm_ids(HouseholdState) ->
    FirmIds = select_simulation_firm_agent_ids(HouseholdState),
    lists:subtract(FirmIds,HouseholdState#household_state.provider_firms_ids).	