-module(household).
-behavior(gen_fsm).

% public API
-export([start/1, start_link/1, daily_step/3, first_day_of_month/3, last_day_of_month/3]).

%% gen_fsm callbacks
-export([init/1,
handle_event/3, handle_sync_event/4, handle_info/3,terminate/3, code_change/4,
 %custom state names
normal/2]).

-include_lib("record_defs.hrl").

%%% PUBLIC API
start(HouseholdState) ->
	gen_fsm:start({local, household_state:get_value(household_id_as_atom, HouseholdState) }, ?MODULE, HouseholdState, []).
start_link(HouseholdState) ->
	gen_fsm:start_link({local, household_state:get_value(household_id_as_atom, HouseholdState) }, ?MODULE, HouseholdState, []).

%%FSM PUBLIC API FUNCTIONS
daily_step(HouseholdId, DayNumber, SimState) ->
	io:format("Processing day ~w for household id: ~w~n",[DayNumber, HouseholdId]),
	spend(HouseholdId, SimState).%%TODO: REMOVE THIS
first_day_of_month(HouseholdId, MonthNumber, SimState) ->
	io:format("Processing first day of month: ~w for household id: ~w~n",[MonthNumber, HouseholdId]).
last_day_of_month(HouseholdId, MonthNumber, SimState) ->
	io:format("Processing last day of month: ~w for household id: ~w~n",[MonthNumber, HouseholdId]).
	
fire_employee(HouseholdId) ->
	io:format("Firing employee with Id: ~w~n",[HouseholdId]),
	gen_fsm:send_event(household_state:household_id_to_atom(HouseholdId), fire_employee). %We do not care about return value

%Private functions
spend(HouseholdId, SimState) -> %%TODO: REMOVE THIS
	io:format("performing a spend: ~n",[]),
	gen_fsm:send_event(household_state:household_id_to_atom(HouseholdId), {spend, SimState}).
payrise(HouseholdId, NewWage) -> %%TODO: REMOVE THIS?
	io:format("giving a payrise to household. new wage: ~w~n",[NewWage]),
	gen_fsm:send_event(household_state:household_id_to_atom(HouseholdId), {payrise,NewWage}).

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
		{spend, SimState}-> %%TODO: REMOVE THIS
			DaysInOneMonth = SimState#sim_state.days_in_one_month,
			Expenditure = State#household_state.planned_monthly_consumption_expenditure/DaysInOneMonth,
			NewLiquidity = State#household_state.liquidity_h-Expenditure,
			io:format("Household ~w is spending ~w; Liquidity gone from: ~w to: ~w~n",[HouseholdId, Expenditure, State#household_state.liquidity_h, NewLiquidity]),			
			{next_state, normal, State#household_state{liquidity_h=NewLiquidity}, 10000};
		fire_employee ->
			{next_state, normal, #household_state{employer_firm_id=0}, 1000};%%TODO: CHECK COMMUNICATION\\TODO: actually fire after one month // TODO: also change state to fired??
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

	







	

