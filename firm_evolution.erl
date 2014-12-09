-module(firm_evolution).

-export([evolve_num_consecutive_months_with_all_positions_filled/1, evolve_wage_rate/1,
			inventory_lower_upper_limits/1, evolve_work_position_has_been_offered/1,
			evolve_num_work_positions/1, evolve_fired_employee_id/1,
			evolve_work_position_has_been_accepted/1, evolve_num_work_positions_filled/1,
			price_lower_upper_limits/1, evolve_price/1,
			evolve_inventory/1, evolve_liquidity_for_salary/1
			]).

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

%TODO:move to include file to share this definition across several modules
-record(firm_state, 
	{
		instance_name, 
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
		employee_ids,
		
		sim_configuration
	}).

evolve_num_consecutive_months_with_all_positions_filled (FirmState) ->
     if 
		FirmState#firm_state.num_work_positions_filled == FirmState#firm_state.num_work_positions_available ->
			FirmState#firm_state.num_consecutive_months_all_work_positions_filled + 1;
		true ->
			0
	 end.
	 
evolve_wage_rate(FirmState) ->
    Mu = numerics:uniform(FirmState#firm_state.sim_configuration#sim_config.wage_growth_rate_uniform_distribution_upper_support),
    EvolvedConsecutiveMonthsWithAllPositionsFilled = evolve_num_consecutive_months_with_all_positions_filled(FirmState),
    if
		(FirmState#firm_state.work_position_has_been_offered == true) and (FirmState#firm_state.work_position_has_been_accepted == false) -> 
			FirmState#firm_state.wage_rate_f * (1.0 + Mu);%increase wage rate
		EvolvedConsecutiveMonthsWithAllPositionsFilled >= FirmState#firm_state.num_consecutive_months_all_work_positions_filled ->          
			FirmState#firm_state.wage_rate_f * (1.0 - Mu);%decrease wage rate
		true ->
			FirmState#firm_state.wage_rate_f %do not change wage rate	 
	end.
	
inventory_lower_upper_limits(FirmState) ->
    {
       FirmState#firm_state.sim_configuration#sim_config.inventory_lower_limit_ratio * FirmState#firm_state.monthly_demand_of_consumption_goods, 
       FirmState#firm_state.sim_configuration#sim_config.price_upper_limit_ratio * FirmState#firm_state.monthly_demand_of_consumption_goods
    }.
	
evolve_work_position_has_been_offered(FirmState) ->
    {InventoryLowerLimit, _} = inventory_lower_upper_limits(FirmState), 
    if 
		FirmState#firm_state.inventory_f < InventoryLowerLimit -> 
			true;
		true -> 
			false	
	end.
	
evolve_num_work_positions(FirmState) -> 
    {InventoryLowerLimit, InventoryUpperLimit} = inventory_lower_upper_limits(FirmState),
	if
		FirmState#firm_state.inventory_f < InventoryLowerLimit ->
			FirmState#firm_state.num_work_positions_available + 1;
		FirmState#firm_state.inventory_f > InventoryUpperLimit ->	
			FirmState#firm_state.num_work_positions_available - 1;
			%TODO: SACK ONE OF THE EMPLOYEES 
		true ->
			FirmState#firm_state.num_work_positions_available	
	end.
	
evolve_fired_employee_id(FirmState) ->
    {_, InventoryUpperLimit} = inventory_lower_upper_limits(FirmState),
	if
		FirmState#firm_state.inventory_f > InventoryUpperLimit ->
			item_selection:choose_random_item(FirmState#firm_state.employee_ids); 
		true ->
			0 %employee_id = 0 means no employee has been fired	
	end.

evolve_work_position_has_been_accepted(FirmState) ->
    FirmState#firm_state.work_position_has_been_accepted. %TODO:this is actually modified in "evolve household" function	
	
evolve_num_work_positions_filled(FirmState) ->
    FirmState#firm_state.num_work_positions_filled. %TODO:this is actually modified in "evolve household" function	
	
price_lower_upper_limits(FirmState) ->
    {
       FirmState#firm_state.sim_configuration#sim_config.price_lower_limit_ratio * FirmState#firm_state.monthly_marginal_costs, 
       FirmState#firm_state.sim_configuration#sim_config.price_upper_limit_ratio * FirmState#firm_state.monthly_marginal_costs
    }.  
	
evolve_price(FirmState) ->
	Ni = numerics:uniform(FirmState#firm_state.sim_configuration#sim_config.price_growth_rate_uniform_distribution_upper_support),
	{InventoryLowerLimit, InventoryUpperLimit} = inventory_lower_upper_limits(FirmState),
	{PriceLowerLimit, PriceUpperLimit} = price_lower_upper_limits(FirmState),
	if 
		FirmState#firm_state.inventory_f < InventoryLowerLimit ->
			if 
				FirmState#firm_state.price_f < PriceUpperLimit ->
					case numerics:is_happening_with_probability(FirmState#firm_state.sim_configuration#sim_config.probability_of_setting_new_price) of
						true -> (1.0 + Ni) * FirmState#firm_state.price_f;
						false -> FirmState#firm_state.price_f
					end;
				true ->
					FirmState#firm_state.price_f
			end;
		FirmState#firm_state.inventory_f > InventoryUpperLimit ->
			if 
				FirmState#firm_state.price_f < PriceLowerLimit ->
					case numerics:is_happening_with_probability(FirmState#firm_state.sim_configuration#sim_config.probability_of_setting_new_price) of
						true -> (1.0 - Ni) * FirmState#firm_state.price_f;
						false -> FirmState#firm_state.price_f
					end;
				true ->
					FirmState#firm_state.price_f
			end;
		true ->
			FirmState#firm_state.price_f
	end.
	
% ---------------
% DAILY EVOLUTION
% ---------------
evolve_inventory(FirmState) ->
    FirmState#firm_state.inventory_f + 
	FirmState#firm_state.sim_configuration#sim_config.technology_productivity_parameter * (length(FirmState#firm_state.employee_ids)).
	
% ---------------------
% LAST DAY OF THE MONTH
% ---------------------
evolve_liquidity_for_salary(FirmState) ->
    PaidSalaries = (length(FirmState#firm_state.employee_ids)) * FirmState#firm_state.wage_rate_f,
	FirmLiquidity = FirmState#firm_state.liquidity_f,
    if 
		(FirmLiquidity > PaidSalaries) ->
			FirmLiquidity - PaidSalaries;
			%TODO: should redistribute profits to household proportionally to their wealth
		(FirmLiquidity < paidSalaries) ->
			0.0;
			%TODO: we should redistribute debt among households
		true ->
			0.0
	end.