-module(household_evolution).

-export(
		[
			select_simulation_firm_agent_ids/1,
			choose_potential_employer_firm/1,
			choose_random_provider_firm_id/1,
			select_unconnected_firm_ids/1,
			choose_unconnected_firm_id_randomly_weighted_on_employee_count/1,
			evolve_provider_firm_ids/1,
			try_set_new_employer_firm/2,
			evolve_employer_firm_id/1,
			evolve_planned_monthly_consumption_expenditure/1,
			try_to_transact_with_provider_firms/1,
			evolve_liquidity_daily/1,
			evolve_liquidity_from_salary/1,
			evolve_claimed_wage_rate/1
		]
 ).
 
-include_lib("record_defs.hrl").

select_simulation_firm_agent_ids(#household_state{sim_configuration=SimConfirguration}) ->
    sim:get_firm_ids_from_lookup(SimConfirguration#sim_state.firm_employees_lookup).

choose_potential_employer_firm(HouseholdState) ->
    FirmIds = select_simulation_firm_agent_ids(HouseholdState),
    item_selection:choose_random_item(FirmIds).
	
choose_random_provider_firm_id(#household_state{provider_firms_ids=ProviderFirmIds}) ->
    item_selection:choose_random_item(ProviderFirmIds).

select_unconnected_firm_ids(HouseholdState) ->
    FirmIds = select_simulation_firm_agent_ids(HouseholdState),
    lists:subtract(FirmIds,HouseholdState#household_state.provider_firms_ids).	
	
choose_unconnected_firm_id_randomly_weighted_on_employee_count(HouseholdState) ->
	UnconnectedFirmsIds = select_unconnected_firm_ids(HouseholdState),
	UnconnectedFirmsIdsSet = sets:from_list(UnconnectedFirmsIds),
	FirmsWeightedOnEmployeeCount = item_selection:list_of_key_list_pairs_to_list_of_key_length_pairs(HouseholdState#household_state.provider_firms_ids),
	UnconnectedFirmsIdsWeightedOnEmployeeCount = lists:filter(
		fun({FirmId, _EmployeeCount})-> sets:is_element(FirmId, UnconnectedFirmsIdsSet) end, FirmsWeightedOnEmployeeCount),
	RandomFirmId = item_selection:choose_weighted_random_item(UnconnectedFirmsIdsWeightedOnEmployeeCount),
	RandomFirmId.	
	
evolve_provider_firm_ids(HouseholdState) ->
	SimConfiguration = HouseholdState#household_state.sim_configuration,
	IsEventHappening = numerics:is_happening_with_probability(SimConfiguration#sim_state.probability_of_household_picking_new_provider_firm),
	if 
		IsEventHappening ->
			ChosenConnectedProviderFirmId = choose_random_provider_firm_id(HouseholdState),
			ChosenUnconnectedProviderFirmId = choose_unconnected_firm_id_randomly_weighted_on_employee_count(HouseholdState),
			ChosenConnectedProviderFirmPrice = firm:get_fsm_value(price_f, ChosenConnectedProviderFirmId),
			ChosenUnconnectedProviderFirmPrice = firm:get_fsm_value(price_f, ChosenUnconnectedProviderFirmId),
			PricePercentDifference = numerics:percent_difference(ChosenConnectedProviderFirmPrice,ChosenUnconnectedProviderFirmPrice),
			if 
				(PricePercentDifference > SimConfiguration#sim_state.price_threshold_of_household_picking_new_provider_firm) ->
					item_selection:replace_item_in_list(
						HouseholdState#household_state.provider_firms_ids,
						ChosenConnectedProviderFirmId, ChosenUnconnectedProviderFirmId);
				true ->
					HouseholdState#household_state.provider_firms_ids	
			end;
		true ->
			HouseholdState#household_state.provider_firms_ids
	end.
%%TODO(*must further evolve Provider firms based on unfulfilled demand*)

try_set_new_employer_firm(MaxNumAttempts, HouseholdState) ->
	set_new_employer_firm(MaxNumAttempts, 1, HouseholdState).
	
set_new_employer_firm(MaxNumAttempts, AttemptCycle, HouseholdState) ->
	ChosenPotentialEmployerFirmId = choose_potential_employer_firm(HouseholdState),
	[WorkPositionHasBeenOffered, WorkPositionHasBeenAccepted, WageRate] = firm:get_fsm_values(
			[work_position_has_been_offered, work_position_has_been_accepted, wage_rate_f], 
			ChosenPotentialEmployerFirmId),
	if
		WorkPositionHasBeenOffered and (not WorkPositionHasBeenAccepted) and WageRate > HouseholdState#household_state.reservation_wage_rate_h ->
			ChosenPotentialEmployerFirmId;
		true ->
			if
				AttemptCycle < MaxNumAttempts ->
					set_new_employer_firm(MaxNumAttempts, AttemptCycle+1, HouseholdState);
				true ->
					HouseholdState#household_state.employer_firm_id
			end
	end.
	
evolve_employer_firm_id(HouseholdState) ->
	EmployerFirmId = HouseholdState#household_state.employer_firm_id,
	SimConfiguration = HouseholdState#household_state.sim_configuration,
	if 
		EmployerFirmId == 0 -> %unemployed
			try_set_new_employer_firm(SimConfiguration#sim_state.max_number_potential_employers_visited, HouseholdState);
		true ->
			WageRate = firm:get_fsm_value(wage_rate_f, EmployerFirmId), 
			if
				HouseholdState#household_state.reservation_wage_rate_h < WageRate -> %unhappy employee
					try_set_new_employer_firm(1, HouseholdState);
				true ->
					%only visit new potential employer with certain probability
					IsEventHappening = numerics:is_happening_with_probability(SimConfiguration#sim_state.probability_of_household_picking_new_provider_firm),
					if
						IsEventHappening ->
							try_set_new_employer_firm(1, HouseholdState);
						true ->
							EmployerFirmId
					end
			end
	end.
	
evolve_planned_monthly_consumption_expenditure(HouseholdState) ->
	SimConfiguration = HouseholdState#household_state.sim_configuration,
	ProviderFirmIds = HouseholdState#household_state.provider_firms_ids,
	ProviderFirmPrices = lists:map(fun(FirmId) -> firm:get_fsm_value(price_f, FirmId) end, ProviderFirmIds),
	AverageGoodsPriceOfProviderFirms = numerics:list_average(ProviderFirmPrices),
	LiquidityRatio = HouseholdState#household_state.liquidity_h / AverageGoodsPriceOfProviderFirms,
	min(math:pow(LiquidityRatio, SimConfiguration#sim_state.planned_consumption_increase_decaying_rate), LiquidityRatio).
	
try_to_transact_with_provider_firms(HouseholdState) ->
	SimConfiguration = HouseholdState#household_state.sim_configuration,
	PlannedDailyConsumptionDemand = HouseholdState#household_state.planned_monthly_consumption_expenditure / SimConfiguration#sim_state.days_in_one_month,
	MaxNumAttempts = SimConfiguration#sim_state.max_number_provider_firms_visited,
	transact_with_provider_firm(1, MaxNumAttempts, PlannedDailyConsumptionDemand, HouseholdState#household_state.liquidity_h, HouseholdState).
	
transact_with_provider_firm(AttemptCycle, MaxNumAttempts, PlannedDailyConsumptionDemand, CurrentHouseholdAgentLiquidity, HouseholdState) ->
	ChosenProviderFirmId = choose_random_provider_firm_id(HouseholdState),
	ChosenProviderFirmInventory = firm:get_fsm_value(inventory_f, ChosenProviderFirmId),
	ChosenProviderFirmPrice = firm:get_fsm_value(price_f, ChosenProviderFirmId),
	case (ChosenProviderFirmInventory > PlannedDailyConsumptionDemand)
		  and (CurrentHouseholdAgentLiquidity >= ChosenProviderFirmPrice * PlannedDailyConsumptionDemand) of
		true ->
			%%TODO: send message to chosenProviderFirm to raise its liquidity by chosenProviderFirm.Price * plannedDailyConsumptionDemand and decrease inventory by plannedDailyConsumptionDemand
			CurrentHouseholdAgentLiquidity - ChosenProviderFirmPrice * PlannedDailyConsumptionDemand;
		false ->
			case (CurrentHouseholdAgentLiquidity < ChosenProviderFirmPrice * PlannedDailyConsumptionDemand) of
				true ->
					AdjustedDailyConsumptionDemand = CurrentHouseholdAgentLiquidity / ChosenProviderFirmPrice,
					%%TODO: send message to chosenProviderFirm to raise its liquidity by chosenProviderFirm.Price * adjustedDailyConsumptionDemand and decrease inventory by adjustedDailyConsumptionDemand
					CurrentHouseholdAgentLiquidity - ChosenProviderFirmPrice * AdjustedDailyConsumptionDemand; %%the result should be 0.0
				false ->
					AdjustedDailyConsumptionDemand = ChosenProviderFirmInventory,
					%%TODO: send message to chosenProviderFirm to raise its liquidity by ChosenProviderFirmInventory * CurrentHouseholdAgentLiquidity and decrease inventory by chosenProviderFirm.Inventory (inventory should go down to 0)
					AmendedHouseholdAgentLiquidity = CurrentHouseholdAgentLiquidity - ChosenProviderFirmInventory * AdjustedDailyConsumptionDemand,
					case (AttemptCycle < MaxNumAttempts) of %%TODO add condition of CurrentHouseholdAgentLiquidity > 5% of agent.Liquidity							
						true-> 
							transact_with_provider_firm(AttemptCycle+1, MaxNumAttempts, PlannedDailyConsumptionDemand, AmendedHouseholdAgentLiquidity, HouseholdState);
						false ->
							AmendedHouseholdAgentLiquidity
					end
			end
	end.

evolve_liquidity_daily(HouseholdState) ->
    try_to_transact_with_provider_firms(HouseholdState).
	
evolve_liquidity_from_salary(HouseholdState) ->
    %%TODO: Employer of household to pay salary and reduice its liquidity: replace line below with single method whihc reduces firm liquidity and returns WageRate
	EmployerFirmId = HouseholdState#household_state.employer_firm_id,
	WageRate = firm:get_fsm_value(wage_rate_f, EmployerFirmId),	
    HouseholdState#household_state.liquidity_h + WageRate.

evolve_claimed_wage_rate(HouseholdState) ->
	SimConfiguration = HouseholdState#household_state.sim_configuration,
	EmployerFirmId = HouseholdState#household_state.employer_firm_id,
	WageRate = firm:get_fsm_value(wage_rate_f, EmployerFirmId),
	case (EmployerFirmId == 0) of
		true -> %%unemployed
			(1.0 - SimConfiguration#sim_state.claimed_wage_rate_percentage_reduction_if_unemployed) * HouseholdState#household_state.reservation_wage_rate_h;
		false -> %%employed
			case (WageRate > HouseholdState#household_state.reservation_wage_rate_h) of
				true ->
					WageRate;
				false ->
					HouseholdState#household_state.reservation_wage_rate_h
			end
	end.