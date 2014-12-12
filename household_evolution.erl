-module(household_evolution).

-export(
		[
			select_simulation_firm_agent_ids/1,
			choose_potential_employer_firm/1,
			choose_random_provider_firm_id/1,
			select_unconnected_firm_ids/1
		]
 ).
 
-include_lib("record_defs.hrl").

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