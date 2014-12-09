-module(firm).
-behavior(gen_fsm).

% public API
-export([start/8, start_link/8, daily_step/2, first_day_of_month/2, last_day_of_month/2]).

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
-record(firm_state, 
	{
		instance_name, 
		inventory_f, %i_f
		liquidity_f, %m_f
		wage_rate_f, %w_f
		price_f, %p_f
		num_work_positions_available, 
		num_work_positions_filled,
		work_position_has_been_offered,
		work_position_has_been_accepted,
		num_consecutive_months_all_work_positions_filled,
		fired_employee_id,
		monthly_demand_of_consumption_goods,
		monthly_marginal_costs,		
		employee_ids,
		
		sim_configuration
	}).

%%% PUBLIC API
start(InstanceName, Inventory, Liquidity, WageRate, Price, NumWorkPositionsAvailable, EmployeeIds, SimConfiguration) ->		
	gen_fsm:start({local, list_to_atom(InstanceName)}, ?MODULE, {InstanceName, Inventory, Liquidity, WageRate, Price, NumWorkPositionsAvailable, EmployeeIds, SimConfiguration}, []).
start_link(InstanceName, Inventory, Liquidity, WageRate, Price, NumWorkPositionsAvailable, EmployeeIds, SimConfiguration ) ->
	gen_fsm:start_link({local, list_to_atom(InstanceName)}, ?MODULE, {InstanceName, Inventory, Liquidity, WageRate, Price, NumWorkPositionsAvailable, EmployeeIds, SimConfiguration}, []).

%%FSM PUBLIC API FUNCTIONS
daily_step(InstanceName, DayNumber) ->
	io:format("Processing day ~w for firm ~s~n",[DayNumber, InstanceName]),
	increase_inventory(InstanceName).

first_day_of_month(InstanceName, MonthNumber) ->
	io:format("Processing first day of month: ~w for firm ~s~n",[MonthNumber, InstanceName]).
last_day_of_month(InstanceName, MonthNumber) ->
	io:format("Processing last day of month: ~w for firm ~s~n",[MonthNumber, InstanceName]).

%Private functions
increase_inventory(InstanceName) ->
	io:format("Increasing inventory_f of instance ~s. ~n",[InstanceName]),
	gen_fsm:send_event(list_to_atom(InstanceName), increase_inventory).

%GEN_FSM CALLBACKS
init({InstanceName, Inventory, Liquidity, WageRate, Price, NumWorkPositionsAvailable, EmployeeIds, SimConfiguration}) ->
	io:format("FIRM_FSM initialising state with Name:~s, Inventory:~w, Liquidity:~w, WageRate: ~w, Price: ~w, Number of Labourers:~w, , EmployeeIds: ~w~n",[InstanceName, Inventory, Liquidity, WageRate, Price, NumWorkPositionsAvailable, EmployeeIds]),
	{ok, normal, #firm_state{instance_name=InstanceName, inventory_f=Inventory, liquidity_f=Liquidity, wage_rate_f=WageRate, price_f=Price, num_work_positions_available=NumWorkPositionsAvailable, employee_ids= EmployeeIds, sim_configuration = SimConfiguration}, 2000}.

normal(Event, State) ->
	io:format("Firm ~s state is NORMAL. Inventory:~w, Liquidity:~w, WageRate:~w, Price:~w, Number of Labourers:~w, Employee Ids:~w~n",[State#firm_state.instance_name, State#firm_state.inventory_f, State#firm_state.liquidity_f, State#firm_state.wage_rate_f, State#firm_state.price_f, State#firm_state.num_work_positions_available, State#firm_state.employee_ids]),
	case Event of
		increase_inventory ->
			NewInventory = State#firm_state.inventory_f + State#firm_state.num_work_positions_available * State#firm_state.sim_configuration#sim_config.technology_productivity_parameter,
			io:format("Firm ~s is increasing inventory_f from ~w to ~w~n",[State#firm_state.instance_name,State#firm_state.inventory_f, NewInventory]),
			%REMOVE THIS!{next_state, normal, #firm_state{instance_name=State#firm_state.instance_name, inventory_f=NewInventory, liquidity_f=State#firm_state.liquidity_f, wage_rate_f=State#firm_state.wage_rate_f, price_f=State#firm_state.price_f, num_work_positions_available=State#firm_state.num_work_positions_available, lambda_f=State#firm_state.sim_configuration#sim_config.technology_productivity_parameter, days_in_one_month=State#firm_state.days_in_one_month}, 10000};
			{next_state, normal, State#firm_state{inventory_f=NewInventory}, 10000};
		timeout ->
			io:format("Nothing has happened to NORMAL firm ~s...~n",[State#firm_state.instance_name]),
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







	

