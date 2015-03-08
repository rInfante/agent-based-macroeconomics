-module(household).
-behavior(gen_fsm).

% public API
-export([start/1, start_link/1, daily_step/3, first_day_of_month/3, last_day_of_month/3, fire_employee/1]).

%% gen_fsm callbacks
-export([init/1,
handle_event/3, handle_sync_event/4, handle_info/3,terminate/3, code_change/4,
%%custom state names
normal/2]).

-include_lib("record_defs.hrl").

%%% PUBLIC API
start(HouseholdState) ->
	gen_fsm:start({local, household_state:get_value(household_id_as_atom, HouseholdState) }, ?MODULE, HouseholdState, []).
start_link(HouseholdState) ->
	gen_fsm:start_link({local, household_state:get_value(household_id_as_atom, HouseholdState) }, ?MODULE, HouseholdState, []).

%%FSM PUBLIC API FUNCTIONS
first_day_of_month(HouseholdId, MonthNumber, SimState) ->
	io:format("Processing first day of month: ~w for household id: ~w~n",[MonthNumber, HouseholdId]),
	evolve_provider_firm_ids(HouseholdId, SimState),
	evolve_employer_firm_id(HouseholdId, SimState),
	evolve_planned_monthly_consumption_expenditure(HouseholdId, SimState).

daily_step(HouseholdId, DayNumber, SimState) ->
	io:format("Processing day ~w for household id: ~w~n",[DayNumber, HouseholdId]),
	evolve_liquidity_from_daily_purchases(HouseholdId, SimState).
	
last_day_of_month(HouseholdId, MonthNumber, SimState) ->
	io:format("Processing last day of month: ~w for household id: ~w~n",[MonthNumber, HouseholdId]).
	
fire_employee(HouseholdId) ->
	io:format("Firing employee with Id: ~w~n",[HouseholdId]),
	gen_fsm:send_event(household_state:household_id_to_atom(HouseholdId), fire_employee). %We do not care about return value

%Private functions
payrise(HouseholdId, NewWage) -> %%TODO: REMOVE THIS?
	io:format("giving a payrise to household. new wage: ~w~n",[NewWage]),
	gen_fsm:send_event(household_state:household_id_to_atom(HouseholdId), {payrise,NewWage}).
evolve_provider_firm_ids(HouseholdId, SimState) ->
	io:format("Evolving provider firm ids for Household with Id: ~w. ~n",[HouseholdId]),
	gen_fsm:send_event(household_state:household_id_to_atom(HouseholdId), {evolve_provider_firm_ids, SimState}).
evolve_employer_firm_id(HouseholdId, SimState) ->
	io:format("Evolving employer firm id for Household with Id: ~w. ~n",[HouseholdId]),
	gen_fsm:send_event(household_state:household_id_to_atom(HouseholdId), {evolve_employer_firm_id, SimState}).	
evolve_planned_monthly_consumption_expenditure(HouseholdId, SimState) ->
	io:format("Evolving planned_monthly_consumption_expenditure for Household with Id: ~w. ~n",[HouseholdId]),
	gen_fsm:send_event(household_state:household_id_to_atom(HouseholdId), {planned_monthly_consumption_expenditure, SimState}).		
	
evolve_liquidity_from_daily_purchases(HouseholdId, SimState) ->
	io:format("Evolving liquidity_h for Household with Id: ~w. ~n",[HouseholdId]),
	gen_fsm:send_event(household_state:household_id_to_atom(HouseholdId), {evolve_liquidity_from_daily_purchases, SimState}).
	
%GEN_FSM CALLBACKS
init(State) ->
	io:format("HOUSEHOLD_FSM initialising household_state with Id:~w, ReservationWageRate:~w, Liquidity:~w, Monthly Demand~w~n",
		household_state:get_values([household_id, reservation_wage_rate_h, liquidity_h, planned_monthly_consumption_expenditure], State)),	
	{ok, normal, State, 2000}.

normal(Event, State) ->
	HouseholdId = household_state:get_value(household_id, State),
	io:format("Household id:~w household_state is NORMAL.  ReservationWageRate:~w, Liquidity:~w, Monthly Demand: ~w~n",
		household_state:get_values([household_id, reservation_wage_rate_h, liquidity_h, planned_monthly_consumption_expenditure], State)),	
	case Event of
		{payrise, NewWage} -> %%TODO: REMOVE THIS?
			CurrentWage = household_state:get_value(reservation_wage_rate_h, State),
			io:format("Household id:~w got payrise... from ~w to ~w~n",[HouseholdId, CurrentWage, NewWage]),
			{next_state, normal, #household_state{reservation_wage_rate_h=NewWage}, 1000};

		fire_employee ->
			io:format("Household id:~w got employee just fired~n",[HouseholdId]),
			{next_state, normal, #household_state{employer_firm_id=0}, 1000};%%TODO: CHECK COMMUNICATION\\TODO: actually fire after one month // TODO: also change state to fired?? (i.e.: a fired employee should no longer earn money)
			
		{evolve_provider_firm_ids, SimState} ->
			ProviderFirmIds = household_state:get_value(provider_firms_ids, SimState),
			NewProviderFirmIds = household_evolution:evolve_provider_firm_ids(State, SimState),
			io:format("Household id:~w is changing provider_firms_ids from ~w to ~w~n",[HouseholdId, ProviderFirmIds, NewProviderFirmIds]),
			{next_state, normal, State#household_state{provider_firms_ids=NewProviderFirmIds}, 10000};
		{evolve_employer_firm_id, SimState} ->
			EmployerFirmId = household_state:get_value(employer_firm_id, SimState),
			NewEmployerFirmId = household_evolution:evolve_employer_firm_id(State, SimState),
			io:format("Household id:~w is changing evolve_employer_firm_id from ~w to ~w~n",[HouseholdId, EmployerFirmId, NewEmployerFirmId]),
			{next_state, normal, State#household_state{employer_firm_id=NewEmployerFirmId}, 10000};			
		{evolve_planned_monthly_consumption_expenditure, SimState} ->
			PlannedMonthlyConsumptionExpenditure = household_state:get_value(planned_monthly_consumption_expenditure, SimState),
			NewPlannedMonthlyConsumptionExpenditure = household_evolution:evolve_planned_monthly_consumption_expenditure(State, SimState),
			io:format("Household id:~w is changing evolve_planned_monthly_consumption_expenditure from ~w to ~w~n",[HouseholdId, PlannedMonthlyConsumptionExpenditure, NewPlannedMonthlyConsumptionExpenditure]),
			{next_state, normal, State#household_state{planned_monthly_consumption_expenditure=NewPlannedMonthlyConsumptionExpenditure}, 10000};
			
		{evolve_liquidity_from_daily_purchases, SimState}-> 
			Liquidity = household_state:get_value(liquidity_h, SimState),
			NewLiquidity = household_evolution:evolve_liquidity_from_daily_purchases(State, SimState),
			io:format("Household id:~w is changing liquidity_h from ~w to ~w~n",[HouseholdId, Liquidity, NewLiquidity]),
			{next_state, normal, State#household_state{liquidity_h=NewLiquidity}, 10000};			
		timeout ->
			io:format("Nothing has happened to NORMAL household id:~w...~n",[HouseholdId]),
			{next_state, normal, State, 10000};
		_ ->
			io:format("Unknown event. Staying NORMAL.~n"),
			{next_state, normal, State, 2000}
	end.


%%GEN FSM BEHAVIOUR TEMPLATE
handle_event(Event, StateName, State) ->
	io:format("Handling event:~w; StateName:~w, State:~w~n",[Event, StateName, State]),
    {next_state, StateName, State}.
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok, 
    {reply, Reply, StateName, State}.
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.
terminate(_Reason, _StateName, _State) ->
    ok.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

	







	

