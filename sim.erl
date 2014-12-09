-module(sim).
-behavior(gen_fsm).

% public API
-export([start/1, start_link/1, new_step/1]).

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

%%% PUBLIC API
start(DaysInOneMonth) ->
	household_fsm_m:start("HH1",1000, 2000, 200, 1, {1,2,3,4,5,6,7}, DaysInOneMonth),
	household_fsm_m:start("HH2",1500, 2500, 250, 2, {2,3,4,5,6,7,8}, DaysInOneMonth),
	firm_fsm_m:start("FI1",10000,100000, 1500, 50, 20, 3,DaysInOneMonth),
	firm_fsm_m:start("FI2",12000,120000, 1700, 70, 30, 3,DaysInOneMonth),
	gen_fsm:start({local, sim}, ?MODULE, {DaysInOneMonth}, []).
start_link(DaysInOneMonth) ->
	household_fsm_m:start_link("HH1",1000, 2000, 200, 1, {1,2,3,4,5,6,7}, DaysInOneMonth),
	household_fsm_m:start_link("HH2",1500, 2500, 250, 1, {1,2,3,4,5,6,7}, DaysInOneMonth),
	firm_fsm_m:start_link("FI1",10000,100000, 1500, 50, 20, 3,DaysInOneMonth),
	firm_fsm_m:start_link("FI2",12000,120000, 1700, 70, 30, 3,DaysInOneMonth),
	gen_fsm:start_link({local, sim}, ?MODULE, {DaysInOneMonth}, []).

%%FSM PUBLIC API FUNCTIONS
new_step(StepNumber) ->
	io:format("Processing step number: ~w~n",[StepNumber]),
	gen_fsm:send_event(sim, {new_step, StepNumber}).

%GEN_FSM CALLBACKS
init({DaysInOneMonth}) ->
	io:format("initialising SIM state with DaysInOneMonth: ~w~n",[DaysInOneMonth]),
	{ok, normal, #state{days_in_one_month=DaysInOneMonth}, 0}.

normal(Event, State) ->
	io:format("Simulator state is NORMAL. DaysInOneMonth: ~w~n",[State#state.days_in_one_month]),
	case Event of
		{new_step, StepNumber} ->
			io:format("New simulation step: ~w~n",[StepNumber]),
			if 
				StepNumber rem State#state.days_in_one_month == 1.0 ->
					MonthNumber= 1 + trunc(StepNumber / State#state.days_in_one_month),
					io:format("This step is the first day of the month. Month number: ~w~n",[MonthNumber]),	
					household_fsm_m:first_day_of_month(MonthNumber),
					firm_fsm_m:first_day_of_month(MonthNumber);
				true ->
					io:format("This step is NOT the first day of the month.~n",[])
			end,
			if 
				StepNumber rem State#state.days_in_one_month == 0.0 ->
					MonthNumber1= StepNumber / State#state.days_in_one_month,
					io:format("This step is the last day of the month. Month number: ~w~n",[MonthNumber1]),	
					household_fsm_m:last_day_of_month(MonthNumber1),
					firm_fsm_m:last_day_of_month(MonthNumber1);
				true ->
					io:format("This step is NOT the last day of the month.~n",[])
			end,
			household_fsm_m:daily_step(StepNumber),
			firm_fsm_m:daily_step(StepNumber),
			{next_state, normal, State, 10000};
		timeout ->
			io:format("Nothing has happened in the simulator...~n"),
			{next_state, normal, State, 10000};
		_ ->
			io:format("Unknown event. Staying normal.~n"),
			{next_state, normal, State, 1000}
	end.

handle_event(Event, StateName, State) ->
	io:format("Handling event:~w; StateName:~w, State:~w",[Event, StateName, State]),
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