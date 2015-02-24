-module(firm_evolution).

-export([	evolve_num_consecutive_months_with_all_positions_filled/1, evolve_wage_rate/2,
			evolve_work_position_has_been_offered/2,
			evolve_num_work_positions/2, evolve_fired_employee_id/2,
			evolve_work_position_has_been_accepted/1, evolve_num_work_positions_filled/1,
			price_lower_upper_limits/2, evolve_price/2,
			evolve_inventory/2, evolve_liquidity_for_salary/1
			]).

%DEBUG			
%-export([inventory_lower_upper_limits/2]).

-include_lib("record_defs.hrl").

% ---------------------
% FIRST DAY OF THE MONTH
% ---------------------

evolve_num_consecutive_months_with_all_positions_filled(FirmState) ->
	 [NumWorkPositionsFilled, NumWorkPositionsAvailable, NumConsecutiveMonthsWithAllWorkPositionsFilled] = 
		firm_state:get_values([num_work_positions_filled, num_work_positions_available, num_consecutive_months_all_work_positions_filled], FirmState),
     if 
		NumWorkPositionsFilled == NumWorkPositionsAvailable ->
			NumConsecutiveMonthsWithAllWorkPositionsFilled + 1;
		true ->
			0
	 end.
	 
evolve_wage_rate(FirmState, SimState) ->
	[WageGrowthRateUniformDistributionUpperSupport, NumConsecutiveMonthsWithAllPositionsFilledUpperLimit] = 
		sim_state:get_values([wage_growth_rate_uniform_distribution_upper_support, num_consecutive_months_with_all_positions_filled_upper_limit], SimState),
    Mu = numerics:uniform(WageGrowthRateUniformDistributionUpperSupport),   
	[WorkPositionHasBeenOffered, WorkPositionHasBeenAccepted, WageRate, NumConsecutiveMonthsWithAllPositionsFilled] = 
		firm_state:get_values([work_position_has_been_offered, work_position_has_been_accepted, wage_rate_f, num_consecutive_months_all_work_positions_filled], FirmState),
    if
		(WorkPositionHasBeenOffered == true) and (WorkPositionHasBeenAccepted == false) -> 
			numerics:increase(WageRate, Mu);
		NumConsecutiveMonthsWithAllPositionsFilled >= NumConsecutiveMonthsWithAllPositionsFilledUpperLimit ->          
			numerics:decrease(WageRate, Mu);
		true ->
			WageRate %do not change wage rate	 
	end.
	
inventory_lower_upper_limits(FirmState, SimState) ->
	[InventoryLowerLimitRatio, InventoryUpperLimitRatio] = sim_state:get_values([inventory_lower_limit_ratio, inventory_upper_limit_ratio], SimState),
	MonthlyDemandOfConsumptionGoods = firm_state:get_value(monthly_demand_of_consumption_goods, FirmState),
    {
       InventoryLowerLimitRatio * MonthlyDemandOfConsumptionGoods, 
       InventoryUpperLimitRatio * MonthlyDemandOfConsumptionGoods
    }.
	
evolve_work_position_has_been_offered(FirmState, SimState) ->
    {InventoryLowerLimit, _} = inventory_lower_upper_limits(FirmState, SimState), 
	Inventory = firm_state:get_value(inventory_f, FirmState),
    if 
		Inventory < InventoryLowerLimit -> 
			true;
		true -> 
			false	
	end.
	
evolve_fired_employee_id(FirmState, SimState) ->
    {_, InventoryUpperLimit} = inventory_lower_upper_limits(FirmState, SimState),
	[Inventory, EmployeeIds] = firm_state:get_values([inventory_f, employee_ids], FirmState),
	if
		Inventory > InventoryUpperLimit ->
			item_selection:choose_random_item(EmployeeIds); 
		true ->
			0 %employee_id = 0 means no employee has been fired	
	end.	
	
evolve_num_work_positions(FirmState, SimState) -> 
    {InventoryLowerLimit, InventoryUpperLimit} = inventory_lower_upper_limits(FirmState, SimState),
	[Inventory, NumWorkPositionsAvailable] = firm_state:get_values([inventory_f, num_work_positions_available], FirmState),
	if
		Inventory < InventoryLowerLimit ->
			NumWorkPositionsAvailable + 1;
		Inventory > InventoryUpperLimit ->	
			%TODO: SACK ONE OF THE EMPLOYEES AFTER ONE MONTH
			NumWorkPositionsAvailable - 1;		
		true ->
			NumWorkPositionsAvailable	
	end.
	
evolve_work_position_has_been_accepted(FirmState) ->
    FirmState#firm_state.work_position_has_been_accepted. %TODO:this is actually modified in "evolve household" function	
	
evolve_num_work_positions_filled(FirmState) ->
    FirmState#firm_state.num_work_positions_filled. %TODO:this is actually modified in "evolve household" function	
	
price_lower_upper_limits(FirmState, SimState) ->
	[PriceLowerLimitRatio, PriceUpperLimitRatio] = sim_state:get_values([price_lower_limit_ratio, price_upper_limit_ratio], SimState),
	MonthlyMarginalCosts = firm_state:get_value(monthly_marginal_costs, FirmState),
    {
       PriceLowerLimitRatio * MonthlyMarginalCosts, 
       PriceUpperLimitRatio * MonthlyMarginalCosts
    }.  
	
evolve_price(FirmState, SimState) ->
	[PriceGrowthRateUniformDistributionUpperSupport, ProbabilityOfSettingNewPrice] = 
		sim_state:get_values([price_growth_rate_uniform_distribution_upper_support, probability_of_setting_new_price], SimState),
	[Inventory, Price] = firm_state:get_values([inventory_f, price_f], FirmState),
	Ni = numerics:uniform(PriceGrowthRateUniformDistributionUpperSupport),
	{InventoryLowerLimit, InventoryUpperLimit} = inventory_lower_upper_limits(FirmState, SimState),
	{PriceLowerLimit, PriceUpperLimit} = price_lower_upper_limits(FirmState, SimState),
	if 
		Inventory < InventoryLowerLimit ->
			if 
				Price < PriceUpperLimit ->
					numerics:increase_with_probability(ProbabilityOfSettingNewPrice, Price, Ni);
				true ->
					Price
			end;
		Inventory > InventoryUpperLimit ->
			if 
				Price > PriceLowerLimit ->
					numerics:decrease_with_probability(ProbabilityOfSettingNewPrice, Price, Ni);
				true ->
					Price
			end;
		true ->
			Price
	end.
	
	
	
% ---------------
% DAILY EVOLUTION
% ---------------
evolve_inventory(FirmState, SimState) ->
	TechnologyProductivityParameter = sim_state:get_value(technology_productivity_parameter, SimState),
	[Inventory, EmployeeIds] = firm_state:get_values([inventory_f, employee_ids], FirmState),
    Inventory + TechnologyProductivityParameter * (length(EmployeeIds)).
	
% ---------------------
% LAST DAY OF THE MONTH
% ---------------------
evolve_liquidity_for_salary(FirmState) ->
	[EmployeeIds, WageRate, FirmLiquidity] = firm_state:get_values([employee_ids, wage_rate_f, liquidity_f], FirmState),
    PaidSalaries = (length(EmployeeIds)) * WageRate,
    if 
		FirmLiquidity > PaidSalaries ->			
			%TODO: should redistribute profits to household proportionally to their wealth
			FirmLiquidity - PaidSalaries;
		FirmLiquidity < paidSalaries ->
			%TODO: we should redistribute debt among households
			0.0;			
		true ->
			0.0
	end.
