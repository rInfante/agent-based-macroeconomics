-record(sim_state, 
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
		
		firm_employees_lookup,
		household_ids
    }).	
	
-record(firm_state, 
	{
		firm_id, 
		inventory_f, %i_f
		liquidity_f, %m_f
		wage_rate_f, %w_f
		price_f, %p_f
		num_work_positions_available, 
		num_work_positions_filled,
		work_position_has_been_offered,
		work_position_has_been_accepted,
		num_consecutive_months_all_work_positions_filled,
		fired_employee_id,
		monthly_demand_of_consumption_goods,
		monthly_marginal_costs,		
		employee_ids
	}).	
	
-record(household_state, 
	{
		household_id, 
		reservation_wage_rate_h, %w_h
		liquidity_h, %m_h
		planned_monthly_consumption_expenditure, %c_r_h
		provider_firms_ids, %type A firms
		employer_firm_id % type B firm
	}).