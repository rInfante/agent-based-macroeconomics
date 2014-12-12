-module(household).
-behavior(gen_fsm).

% public API
-export([start/7, start_link/7, daily_step/2, first_day_of_month/2, last_day_of_month/2]).

%% gen_fsm callbacks
-export([init/1,
handle_event/3, handle_sync_event/4, handle_info/3,terminate/3, code_change/4,
 %cusatom state names
normal/2]).

-include_lib("record_defs.hrl").

%%% PUBLIC API
start(HouseholdId, ReservationWageRate, Liquidity, PlannedMonthlyConsumptionExpenditure, ProviderFirmsIds, EmployerFirmId, SimConfiguration) ->
	gen_fsm:start({local, list_to_atom(HouseholdId)}, ?MODULE, {HouseholdId, ReservationWageRate, Liquidity, PlannedMonthlyConsumptionExpenditure, ProviderFirmsIds, EmployerFirmId, SimConfiguration}, []).
start_link(HouseholdId, ReservationWageRate, Liquidity, PlannedMonthlyConsumptionExpenditure, ProviderFirmsIds, EmployerFirmId, SimConfiguration) ->
	gen_fsm:start_link({local, list_to_atom(HouseholdId)}, ?MODULE, {HouseholdId, ReservationWageRate, Liquidity, PlannedMonthlyConsumptionExpenditure, ProviderFirmsIds, EmployerFirmId, SimConfiguration}, []).

%%FSM PUBLIC API FUNCTIONS
daily_step(HouseholdId, DayNumber) ->
	io:format("Processing day ~w for household id: ~w~n",[DayNumber, HouseholdId]),
	spend(HouseholdId).

first_day_of_month(HouseholdId, MonthNumber) ->
	io:format("Processing first day of month: ~w for household id: ~w~n",[MonthNumber, HouseholdId]).
last_day_of_month(HouseholdId, MonthNumber) ->
	io:format("Processing last day of month: ~w for household id: ~w~n",[MonthNumber, HouseholdId]).


%Private functions
spend(HouseholdId) ->
	io:format("performing a spend: ~n",[]),
	gen_fsm:send_event(household_id_to_atom(HouseholdId), spend).
payrise(HouseholdId, NewWage) ->
	io:format("giving a payrise to household. new wage: ~w~n",[NewWage]),
	gen_fsm:send_event(household_id_to_atom(HouseholdId), {payrise,NewWage}).

%GEN_FSM CALLBACKS
init({HouseholdId, ReservationWageRate, Liquidity, PlannedMonthlyConsumptionExpenditure, ProviderFirmsIds, EmployerFirmId, SimConfiguration}) ->
	io:format("HOUSEHOLD_FSM initialising household_state with Id:~w, ReservationWageRate:~w, Liquidity:~w, Monthly Demand: ~w, Type A Company Ids: ~w, Type B Company Id: ~w~n",[HouseholdId, ReservationWageRate, Liquidity, PlannedMonthlyConsumptionExpenditure, ProviderFirmsIds, EmployerFirmId]),
	{ok, normal, #household_state{household_id=HouseholdId, reservation_wage_rate_h=ReservationWageRate, liquidity_h=Liquidity, planned_monthly_consumption_expenditure=PlannedMonthlyConsumptionExpenditure, provider_firms_ids=ProviderFirmsIds, employer_firm_id=EmployerFirmId, sim_configuration=SimConfiguration}, 2000}.

normal(Event, State) ->
	io:format("Household id:~w household_state is NORMAL.  ReservationWageRate:~w, Liquidity:~w, Monthly Demand: ~w~n",[State#household_state.household_id, State#household_state.reservation_wage_rate_h, State#household_state.liquidity_h, State#household_state.planned_monthly_consumption_expenditure]),
	case Event of
		{payrise, NewWage} ->
			io:format("Household id:~w got payrise... from ~w to ~w~n",[State#household_state.household_id, State#household_state.reservation_wage_rate_h, NewWage]),
			{next_state, normal, #household_state{reservation_wage_rate_h=NewWage}, 1000};
		spend ->
			Expenditure = State#household_state.planned_monthly_consumption_expenditure/State#household_state.sim_configuration#sim_config.days_in_one_month,
			io:format("Household ~w is spending ~w~n",[State#household_state.household_id, Expenditure]),			
			{next_state, normal, #household_state{liquidity_h=State#household_state.liquidity_h-Expenditure}, 10000};
		timeout ->
			io:format("Nothing has happened to NORMAL household id:~w...~n",[State#household_state.household_id]),
			{next_state, normal, State, 10000};
		_ ->
			io:format("Unknown event. Staying NORMAL.~n"),
			{next_state, normal, State, 2000}
	end.


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
	
%%PRIVATE
household_id_to_str(HouseholdId) ->
	string:concat("HH", integer_to_list(HouseholdId)).
	
household_id_to_atom(HouseholdId) ->
	HouseholdIdStr = household_id_to_str(HouseholdId),
	list_to_atom(HouseholdIdStr).	







	

