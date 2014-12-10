-module(household).
-behavior(gen_fsm).

% public API
-export([start/7, start_link/7, daily_step/2, first_day_of_month/2, last_day_of_month/2]).

%% gen_fsm callbacks
-export([init/1,
handle_event/3, handle_sync_event/4, handle_info/3,terminate/3, code_change/4,
 %cusatom state names
normal/2]).

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
-record(household_state, 
	{
		instance_name, 
		reservation_wage_rate_h, %w_h
		liquidity_h, %m_h
		planned_monthly_consumption_expenditure, %c_r_h
		provider_firms_ids, %type A firms
		employer_firm_id, % type B firm
		
		sim_configuration
	}).

%%% PUBLIC API
start(InstanceName, ReservationWageRate, Liquidity, PlannedMonthlyConsumptionExpenditure, ProviderFirmsIds, EmployerFirmId, SimConfiguration) ->
	gen_fsm:start({local, list_to_atom(InstanceName)}, ?MODULE, {InstanceName, ReservationWageRate, Liquidity, PlannedMonthlyConsumptionExpenditure, ProviderFirmsIds, EmployerFirmId, SimConfiguration}, []).
start_link(InstanceName, ReservationWageRate, Liquidity, PlannedMonthlyConsumptionExpenditure, ProviderFirmsIds, EmployerFirmId, SimConfiguration) ->
	gen_fsm:start_link({local, list_to_atom(InstanceName)}, ?MODULE, {InstanceName, ReservationWageRate, Liquidity, PlannedMonthlyConsumptionExpenditure, ProviderFirmsIds, EmployerFirmId, SimConfiguration}, []).

%%FSM PUBLIC API FUNCTIONS
daily_step(InstanceName, DayNumber) ->
	io:format("Processing day ~w for firm ~s~n",[DayNumber, InstanceName]),
	spend(InstanceName).

first_day_of_month(InstanceName, MonthNumber) ->
	io:format("Processing first day of month: ~w for firm ~s~n",[MonthNumber, InstanceName]).
last_day_of_month(InstanceName, MonthNumber) ->
	io:format("Processing last day of month: ~w for firm ~s~n",[MonthNumber, InstanceName]).


%Private functions
spend(InstanceName) ->
	io:format("performing a spend: ~n",[]),
	gen_fsm:send_event(list_to_atom(InstanceName), spend).
payrise(InstanceName, NewWage) ->
	io:format("giving a payrise to household. new wage: ~w~n",[NewWage]),
	gen_fsm:send_event(list_to_atom(InstanceName), {payrise,NewWage}).

%GEN_FSM CALLBACKS
init({InstanceName, ReservationWageRate, Liquidity, PlannedMonthlyConsumptionExpenditure, ProviderFirmsIds, EmployerFirmId, SimConfiguration}) ->
	io:format("HOUSEHOLD_FSM initialising household_state with Name:~s, ReservationWageRate:~w, Liquidity:~w, Monthly Demand: ~w, Type A Company Ids: ~w, Type B Company Id: ~w~n",[InstanceName, ReservationWageRate, Liquidity, PlannedMonthlyConsumptionExpenditure, ProviderFirmsIds, EmployerFirmId]),
	{ok, normal, #household_state{instance_name=InstanceName, reservation_wage_rate_h=ReservationWageRate, liquidity_h=Liquidity, planned_monthly_consumption_expenditure=PlannedMonthlyConsumptionExpenditure, provider_firms_ids=ProviderFirmsIds, employer_firm_id=EmployerFirmId, sim_configuration=SimConfiguration}, 2000}.

normal(Event, State) ->
	io:format("Household ~s household_state is NORMAL.  ReservationWageRate:~w, Liquidity:~w, Monthly Demand: ~w~n",[State#household_state.instance_name, State#household_state.reservation_wage_rate_h, State#household_state.liquidity_h, State#household_state.planned_monthly_consumption_expenditure]),
	case Event of
		{payrise, NewWage} ->
			io:format("Household ~s got payrise... from ~w to ~w~n",[State#household_state.instance_name, State#household_state.reservation_wage_rate_h, NewWage]),
			{next_state, normal, #household_state{reservation_wage_rate_h=NewWage}, 1000};
		spend ->
			Expenditure = State#household_state.planned_monthly_consumption_expenditure/State#household_state.sim_configuration#sim_config.days_in_one_month,
			io:format("Household ~s is spending ~w~n",[State#household_state.instance_name, Expenditure]),
			%REMOVE THIS!{next_state, normal, #household_state{instance_name=State#household_state.instance_name, reservation_wage_rate_h=State#household_state.reservation_wage_rate_h, liquidity_h=State#household_state.liquidity_h-Expenditure, planned_monthly_consumption_expenditure=State#household_state.planned_monthly_consumption_expenditure, provider_firms_ids=State#household_state.provider_firms_ids, employer_firm_id=State#household_state.employer_firm_id, days_in_one_month=State#household_state.days_in_one_month}, 10000};
			{next_state, normal, #household_state{liquidity_h=State#household_state.liquidity_h-Expenditure}, 10000};
		timeout ->
			io:format("Nothing has happened to NORMAL household ~s...~n",[State#household_state.instance_name]),
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







	

