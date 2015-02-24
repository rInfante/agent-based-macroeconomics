-module(firm).
-behavior(gen_fsm).

% public API
-export([start/1, start_link/1, daily_step/3, first_day_of_month/3, last_day_of_month/3, get_fsm_value/2, get_fsm_values/2]).

%% gen_fsm callbacks
-export([init/1,
handle_event/3, handle_sync_event/4, handle_info/3,terminate/3, code_change/4,
 %custom state names
normal/2, normal/3]).

-include_lib("record_defs.hrl").	

%%% PUBLIC API
start(FirmState) ->		
	gen_fsm:start({local, firm_state:get_value(firm_id_as_atom, FirmState)}, ?MODULE, FirmState, []).
start_link(FirmState) ->
	gen_fsm:start_link({local, firm_state:get_value(firm_id_as_atom, FirmState)}, ?MODULE, FirmState, []).

%%FSM PUBLIC API FUNCTIONS

first_day_of_month(FirmId, MonthNumber, SimState) ->
	io:format("Processing first day of month: ~w for firm id:~w~n",[MonthNumber, FirmId]),
	evolve_num_consecutive_months_with_all_positions_filled(FirmId),
	evolve_wage_rate(FirmId, SimState),
	evolve_work_positions(FirmId, SimState).
daily_step(FirmId, DayNumber, SimState) ->
	io:format("Processing day ~w for firm id:~w~n",[DayNumber, FirmId]),
	increase_inventory(FirmId, SimState).	%%TODO: CHECK IF NEEDED
last_day_of_month(FirmId, MonthNumber, SimState) ->
	io:format("Processing last day of month: ~w for firm id:~w~n",[MonthNumber, FirmId]).
	
get_fsm_value(Arg, FirmId) -> 
	FirmState = gen_fsm:sync_send_event(FirmId, get_state),%%TODO: CHECK IF CORRECT!!! (Missing From?)
	firm_state:get_value(Arg, FirmState).
get_fsm_values(Args, FirmId) -> 
	FirmState = gen_fsm:sync_send_event(FirmId, get_state),%%TODO: CHECK IF CORRECT!!! (Missing From?)
	firm_state:get_values(Args, FirmState).	
	
%Private functions

increase_inventory(FirmId, SimState) -> %%TODO: confirm this
	io:format("Increasing inventory_f of instance ~w. ~n",[FirmId]),
	gen_fsm:send_event(firm_state:firm_id_to_atom(FirmId), {increase_inventory, SimState}).
evolve_num_consecutive_months_with_all_positions_filled(FirmId) ->
	io:format("Evolving evolve_num_consecutive_months_with_all_positions_filled of instance ~w. ~n",[FirmId]),
	gen_fsm:send_event(firm_state:firm_id_to_atom(FirmId), evolve_num_consecutive_months_with_all_positions_filled).
evolve_wage_rate(FirmId, SimState) ->
	io:format("Evolving wage_rate_f of instance ~w. ~n",[FirmId]),
	gen_fsm:send_event(firm_state:firm_id_to_atom(FirmId), {evolve_wage_rate, SimState}).
evolve_work_positions(FirmId, SimState) ->	
	io:format("Evolving work num_work_positions_available of instance ~w. ~n",[FirmId]),
	gen_fsm:send_event(firm_state:firm_id_to_atom(FirmId), {evolve_work_positions, SimState}).
	
%GEN_FSM CALLBACKS
init(State) ->
	io:format("FIRM_FSM initialising state with Id:~w, Inventory:~w, Liquidity:~w, WageRate: ~w, Price: ~w, Number of Labourers:~w, EmployeeIds: ~w~n",
		firm_state:get_values([firm_id, inventory_f, liquidity_f, wage_rate_f, price_f, num_work_positions_available, employee_ids], State)),		
	{ok, normal, State, 2000}.

normal(Event, State) ->
	FirmId = firm_state:get_value(firm_id, State),
	io:format("Firm ~w state is NORMAL. Inventory:~w, Liquidity:~w, WageRate:~w, Price:~w, Number of Labourers:~w, Employee Ids:~w~n",
		firm_state:get_values([firm_id, inventory_f, liquidity_f, wage_rate_f, price_f, num_work_positions_available, employee_ids], State)),	
	case Event of
		{increase_inventory, SimState} -> %%TODO : CONFIRM THIS
			%%TODO: change this to call external functions
			TechnologyProductivityParameter = sim_state:get_value(technology_productivity_parameter, SimState),
			[Inventory, NumWorkPositionsAvailable] = firm_state:get_values([inventory_f, num_work_positions_available], State),
			NewInventory = Inventory + NumWorkPositionsAvailable * TechnologyProductivityParameter,
			io:format("Firm id:~w is increasing inventory_f from ~w to ~w~n",[FirmId, Inventory, NewInventory]),			
			{next_state, normal, State#firm_state{inventory_f=NewInventory}, 10000};
		evolve_num_consecutive_months_with_all_positions_filled ->
			NumConsecutiveMonthsWithAllPositionsFilled = firm_state:get_value(num_consecutive_months_with_all_positions_filled, State),
			NewNumConsecutiveMonthsWithAllPositionsFilled = firm_evolution:evolve_num_consecutive_months_with_all_positions_filled(State),
			io:format("Firm id:~w is changing num_consecutive_months_with_all_positions_filled from ~w to ~w~n",[FirmId, NumConsecutiveMonthsWithAllPositionsFilled, NewNumConsecutiveMonthsWithAllPositionsFilled]),
			{next_state, normal, State#firm_state{num_consecutive_months_all_work_positions_filled=NewNumConsecutiveMonthsWithAllPositionsFilled}, 10000};	
		{evolve_wage_rate, SimState} ->
			WageRate = firm_state:get_value(wage_rate_f, State),
			NewWageRate = firm_evolution:evolve_wage_rate(State, SimState),
			io:format("Firm id:~w is changing wage_rate_f from ~w to ~w~n",[FirmId, WageRate, NewWageRate]),
			%We do not increase the salary of the current employees
			{next_state, normal, State#firm_state{wage_rate_f=NewWageRate}, 10000};	
		{evolve_work_positions, SimState} ->
			[WorkPositionHasBeenOffered, FiredEmployeeId, NumWorkPositionsAvailable] = 
				firm_state:get_values([work_position_has_been_offered, fired_employee_id, num_work_positions_available], State),
			NewWorkPositionHasBeenOffered = firm_evolution:evolve_work_position_has_been_offered(State, SimState),
			NewFiredEmployeeId = firm_evolution:evolve_fired_employee_id(State, SimState),
			NewNumWorkPositionsAvailable = firm_evolution:evolve_num_work_positions(State, SimState),
			io:format("Firm id:~w is changing work_position_has_been_offered from ~w to ~w; fired_employee_id from ~w to ~w; num_work_positions_available from ~w to ~w ~n",
				[FirmId, WorkPositionHasBeenOffered, NewWorkPositionHasBeenOffered, FiredEmployeeId, NewFiredEmployeeId, NumWorkPositionsAvailable, NewNumWorkPositionsAvailable]),
			household:fire_employeee(NewFiredEmployeeId),%%PUT IF CONDITION ON NewFiredEmployeeId /= 0 AND RETURN A VALUE EVEN IF NOT NEEDED
			{next_state, normal, State#firm_state{work_position_has_been_offered=NewWorkPositionHasBeenOffered, fired_employee_id=NewFiredEmployeeId, num_work_positions_available=NewNumWorkPositionsAvailable}, 10000};	
		timeout ->
			io:format("Nothing has happened to NORMAL firm id:~w...~n",[FirmId]),
			{next_state, normal, State, 10000};
		_ ->
			io:format("Unknown event. Staying NORMAL.~n"),
			{next_state, normal, State, 2000}
	end.
normal(Event, From, State) ->
	FirmId = firm_state:get_value(firm_id, State),
	io:format("An event has been sent from: ~w to Firm Id: ~w~n",[From, FirmId]),
	case Event of
		get_state ->
			{reply, State, normal, State};
		timeout ->
			io:format("Nothing has happened to NORMAL firm id:~w...~n",[FirmId]),
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








	

