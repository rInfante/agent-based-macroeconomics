-module(household_evolution).

-export(
		[								
			evolve_provider_firm_ids/2,
			evolve_employer_firm_id/2,
			evolve_planned_monthly_consumption_expenditure/2,
			
			evolve_liquidity_from_daily_purchases/2,
			evolve_liquidity_from_salary/1,
			evolve_claimed_wage_rate/2
		]
 ).
 
 %DEBUG
 -export([
			choose_random_provider_firm_id/1,
			choose_unconnected_firm_id_randomly_weighted_on_employee_count/2,
			select_unconnected_firm_ids/2,
			try_set_new_employer_firm/3,
			try_to_transact_with_provider_firms/2,
			choose_potential_employer_firm/1
		  ]).
 
-include_lib("record_defs.hrl").

% ----------------------
% FIRST DAY OF THE MONTH
% ----------------------
	
%%--	
evolve_provider_firm_ids(HouseholdState, SimState) ->
	ProviderFirmIds = household_state:get_value(provider_firms_ids, HouseholdState),
	[ProbabilityOfHouseholdPickingProviderFirm, PriceThresholdOfHouseholdPickingNewProviderFirm] = 
		sim_state:get_values([probability_of_household_picking_new_provider_firm, price_threshold_of_household_picking_new_provider_firm], SimState),	
	IsEventHappening = numerics:is_happening_with_probability(ProbabilityOfHouseholdPickingProviderFirm),
	if 
		IsEventHappening ->
			ChosenConnectedProviderFirmId = choose_random_provider_firm_id(HouseholdState),
			ChosenUnconnectedProviderFirmId = choose_unconnected_firm_id_randomly_weighted_on_employee_count(HouseholdState, SimState),
			ChosenConnectedProviderFirmPrice = firm:get_fsm_value(price_f, ChosenConnectedProviderFirmId),
			ChosenUnconnectedProviderFirmPrice = firm:get_fsm_value(price_f, ChosenUnconnectedProviderFirmId),
			PricePercentDifference = numerics:percent_difference(ChosenConnectedProviderFirmPrice,ChosenUnconnectedProviderFirmPrice),
			if 
				(PricePercentDifference > PriceThresholdOfHouseholdPickingNewProviderFirm) ->
					item_selection:replace_item_in_list(ProviderFirmIds, ChosenConnectedProviderFirmId, ChosenUnconnectedProviderFirmId);
				true ->
					ProviderFirmIds	
			end;
		true ->
			ProviderFirmIds
	end.
%%TODO(*must further evolve Provider firms based on unfulfilled demand of some providers: beginning of page 11*)

choose_unconnected_firm_id_randomly_weighted_on_employee_count(HouseholdState, SimState) ->
	UnconnectedFirmsIds = select_unconnected_firm_ids(HouseholdState, SimState),
	UnconnectedFirmsIdsSet = sets:from_list(UnconnectedFirmsIds),
	ProviderFirmIds = household_state:get_value(provider_firms_ids, HouseholdState),
	FirmsWeightedOnEmployeeCount = item_selection:list_of_key_list_pairs_to_list_of_key_length_pairs(ProviderFirmIds),
	UnconnectedFirmsIdsWeightedOnEmployeeCount = lists:filter(
		fun({FirmId, _EmployeeCount})-> sets:is_element(FirmId, UnconnectedFirmsIdsSet) end, FirmsWeightedOnEmployeeCount),
	RandomFirmId = item_selection:choose_weighted_random_item(UnconnectedFirmsIdsWeightedOnEmployeeCount),
	RandomFirmId.	

select_unconnected_firm_ids(HouseholdState, SimState) ->
    FirmIds = sim_state:get_value(firm_ids, SimState),
	ProviderFirmIds = household_state:get_value(provider_firms_ids, HouseholdState),
    lists:subtract(FirmIds,ProviderFirmIds).	

choose_random_provider_firm_id(HouseholdState) ->
	ProviderFirmIds = household_state:get_value(provider_firms_ids, HouseholdState),
    item_selection:choose_random_item(ProviderFirmIds).	

%%--

evolve_employer_firm_id(HouseholdState, SimState) ->
	[EmployerFirmId, HouseholdWageRate] = household:get_values([employer_firm_id, reservation_wage_rate_h], HouseholdState),
	[MaxNumberPotentialEmployersVisited, ProbabilityOfHouseholdPickingNewProviderFirm] = 
		sim_state:get_values([max_number_potential_employers_visited, probability_of_household_picking_new_provider_firm], SimState),
	if 
		EmployerFirmId == 0 -> %unemployed
			try_set_new_employer_firm(MaxNumberPotentialEmployersVisited, HouseholdState, SimState);
		true ->
			FirmWageRate = firm:get_fsm_value(wage_rate_f, EmployerFirmId), 
			if
				HouseholdWageRate < FirmWageRate -> %unhappy employee
					try_set_new_employer_firm(1, HouseholdState, SimState);
				true ->
					%only visit new potential employer with certain probability
					IsEventHappening = numerics:is_happening_with_probability(ProbabilityOfHouseholdPickingNewProviderFirm),
					if
						IsEventHappening ->
							try_set_new_employer_firm(1, HouseholdState, SimState);
						true ->
							EmployerFirmId
					end
			end
	end.

try_set_new_employer_firm(MaxNumAttempts, HouseholdState, SimState) ->
	set_new_employer_firm(MaxNumAttempts, 1, HouseholdState, SimState).
	
set_new_employer_firm(MaxNumAttempts, AttemptCycle, HouseholdState, SimState) ->
	ChosenPotentialEmployerFirmId = choose_potential_employer_firm(SimState),
	[WorkPositionHasBeenOffered, WorkPositionHasBeenAccepted, FirmWageRate] = 
		firm:get_fsm_values([work_position_has_been_offered, work_position_has_been_accepted, wage_rate_f], ChosenPotentialEmployerFirmId),
	[EmployerFirmId, HouseholdWageRate] = household:get_values([employer_firm_id, reservation_wage_rate_h], HouseholdState),
	if
		WorkPositionHasBeenOffered and (not WorkPositionHasBeenAccepted) and FirmWageRate > HouseholdWageRate ->
			ChosenPotentialEmployerFirmId;
		true ->
			if
				AttemptCycle < MaxNumAttempts ->
					set_new_employer_firm(MaxNumAttempts, AttemptCycle+1, HouseholdState, SimState);
				true ->
					EmployerFirmId
			end
	end.
	
choose_potential_employer_firm(SimState) ->
    FirmIds = sim_state:get_value(firm_ids, SimState),
    item_selection:choose_random_item(FirmIds).	

%%--
	
evolve_planned_monthly_consumption_expenditure(HouseholdState, SimState) ->
	[ProviderFirmIds, Liquidity] = household_state:get_values([provider_firms_ids, liquidity_h], HouseholdState),
	PlannedConsumptionIncreaseDecayingRate = sim_state:get_value(planned_consumption_increase_decaying_rate, SimState),
	ProviderFirmPrices = lists:map(fun(FirmId) -> firm:get_fsm_value(price_f, FirmId) end, ProviderFirmIds),
	AverageGoodsPriceOfProviderFirms = numerics:list_average(ProviderFirmPrices),
	LiquidityRatio = Liquidity / AverageGoodsPriceOfProviderFirms,
	min(math:pow(LiquidityRatio, PlannedConsumptionIncreaseDecayingRate), LiquidityRatio).

% ---------------
% DAILY EVOLUTION
% ---------------
	
evolve_liquidity_from_daily_purchases(HouseholdState, SimState) ->
    try_to_transact_with_provider_firms(HouseholdState, SimState).
	
try_to_transact_with_provider_firms(HouseholdState, SimState) ->
	[PlannedMonthlyConsumptionExpenditure, Liquidity] = household_state:get_values([planned_monthly_consumption_expenditure, liquidity_h], HouseholdState),
	[DaysInOneMonth, MaxNumberProviderFirmsVisited] = 
		sim_state:get_values([days_in_one_month, max_number_provider_firms_visited], SimState),		
	PlannedDailyConsumptionDemand = PlannedMonthlyConsumptionExpenditure / DaysInOneMonth,
	MaxNumAttempts = MaxNumberProviderFirmsVisited,
	transact_with_provider_firm(1, MaxNumAttempts, PlannedDailyConsumptionDemand, Liquidity, HouseholdState).
	
transact_with_provider_firm(AttemptCycle, MaxNumAttempts, PlannedDailyConsumptionDemand, CurrentHouseholdAgentLiquidity, HouseholdState) ->
	ChosenProviderFirmId = choose_random_provider_firm_id(HouseholdState),
	ChosenProviderFirmInventory = firm:get_fsm_value(inventory_f, ChosenProviderFirmId),
	ChosenProviderFirmPrice = firm:get_fsm_value(price_f, ChosenProviderFirmId),
	case (ChosenProviderFirmInventory > PlannedDailyConsumptionDemand)
		  and (CurrentHouseholdAgentLiquidity >= ChosenProviderFirmPrice * PlannedDailyConsumptionDemand) of
		true ->
			PurchaseCost = firm:buy_goods(ChosenProviderFirmId, PlannedDailyConsumptionDemand),
			CurrentHouseholdAgentLiquidity - PurchaseCost;
		false ->
			case (CurrentHouseholdAgentLiquidity < ChosenProviderFirmPrice * PlannedDailyConsumptionDemand) of
				true ->
					AdjustedDailyConsumptionDemand = CurrentHouseholdAgentLiquidity div ChosenProviderFirmPrice,
					PurchaseCost = firm:buy_goods(ChosenProviderFirmId, AdjustedDailyConsumptionDemand),
					CurrentHouseholdAgentLiquidity - PurchaseCost; %%the result should be next to 0.0 but not exactly 0.0 (because we don't by half item)
				false ->
					AdjustedDailyConsumptionDemand = ChosenProviderFirmInventory,
					PurchaseCost = firm:buy_goods(ChosenProviderFirmId, AdjustedDailyConsumptionDemand),
					AmendedHouseholdAgentLiquidity = CurrentHouseholdAgentLiquidity - PurchaseCost,
					case (AttemptCycle < MaxNumAttempts) of %%TODO add condition of CurrentHouseholdAgentLiquidity > 5% of agent.Liquidity							
						true-> 
							transact_with_provider_firm(AttemptCycle+1, MaxNumAttempts, PlannedDailyConsumptionDemand, AmendedHouseholdAgentLiquidity, HouseholdState);
						false ->
							AmendedHouseholdAgentLiquidity
					end
			end
	end.

% ----------------------
% LAST DAY OF THE MONTH
% ----------------------
	
evolve_liquidity_from_salary(HouseholdState) ->    
	[EmployerFirmId, Liquidity] = household:get_values([employer_firm_id, liquidity_h], HouseholdState),	
	FirmWageRate = firm:pay_salary(EmployerFirmId),	
    Liquidity + FirmWageRate.

evolve_claimed_wage_rate(HouseholdState, SimState) ->
	ClaimedWageRatePercentageReductionIfUnemployed = sim_state:get_value(claimed_wage_rate_percentage_reduction_if_unemployed, SimState),
	[EmployerFirmId, HouseholdWageRate] = household:get_values([employer_firm_id, reservation_wage_rate_h], HouseholdState),
	FirmWageRate = firm:get_fsm_value(wage_rate_f, EmployerFirmId),
	case (EmployerFirmId == 0) of
		true -> %%unemployed
			(1.0 - ClaimedWageRatePercentageReductionIfUnemployed) * HouseholdWageRate;
		false -> %%employed
			case (FirmWageRate > HouseholdWageRate) of
				true ->
					FirmWageRate;
				false ->
					HouseholdWageRate
			end
	end.