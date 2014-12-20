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
			evolve_planned_monthly_consumption_expenditure/1
		]
 ).
 
-include_lib("record_defs.hrl").

select_simulation_firm_agent_ids(#household_state{sim_configuration=SimConfirguration}) ->
    sim:get_firm_ids_from_lookup(SimConfirguration#sim_config.firm_employees_lookup).

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
	IsEventHappening = numerics:is_happening_with_probability(SimConfiguration#sim_config.probability_of_household_picking_new_provider_firm),
	if 
		IsEventHappening ->
			ChosenConnectedProviderFirmId = choose_random_provider_firm_id(HouseholdState),
			ChosenUnconnectedProviderFirmId = choose_unconnected_firm_id_randomly_weighted_on_employee_count(HouseholdState),
			ChosenConnectedProviderFirmPrice = firm_get_price(ChosenConnectedProviderFirmId),
			ChosenUnconnectedProviderFirmPrice = firm_get_price(ChosenUnconnectedProviderFirmId),
			PricePercentDifference = numerics:percent_difference(ChosenConnectedProviderFirmPrice,ChosenUnconnectedProviderFirmPrice),
			if 
				(PricePercentDifference > SimConfiguration#sim_config.price_threshold_of_household_picking_new_provider_firm) ->
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
	[WorkPositionHasBeenOffered, WorkPositionHasBeenAccepted, WageRate] = firm_get_work_properties(ChosenPotentialEmployerFirmId),
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
			try_set_new_employer_firm(SimConfiguration#sim_config.max_number_potential_employers_visited, HouseholdState);
		true ->
			[_, _, WageRate] = firm_get_work_properties(EmployerFirmId),
			if
				HouseholdState#household_state.reservation_wage_rate_h < WageRate -> %unhappy employee
					try_set_new_employer_firm(1, HouseholdState);
				true ->
					%only visit new potential employer with certain probability
					IsEventHappening = numerics:is_happening_with_probability(SimConfiguration#sim_config.probability_of_household_picking_new_provider_firm),
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
	ProviderFirmPrices = lists:map(fun(FirmId) -> firm_get_price(FirmId) end, ProviderFirmIds),
	AverageGoodsPriceOfProviderFirms = numerics:list_average(ProviderFirmPrices),
	LiquidityRatio = HouseholdState#household_state.liquidity_h / AverageGoodsPriceOfProviderFirms,
	min(math:pow(LiquidityRatio, SimConfiguration#sim_config.planned_consumption_increase_decaying_rate), LiquidityRatio).
	
%%PRIVATE	
%%communication other agents
%%TODO: possibly move to other module?

firm_get_price(FirmId) ->
	gen_fsm:sync_send_event(FirmId, get_price).
	
firm_get_work_properties(FirmId) ->
	gen_fsm:sync_send_event(FirmId, get_work_properties).