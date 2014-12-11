-module(sim).
-behavior(gen_fsm).

% public API
-export([start/20, start_link/20, new_step/1]).

%% gen_fsm callbacks
-export([init/1,
handle_event/3, handle_sync_event/4, handle_info/3,terminate/3, code_change/4,
 %cusatom sim_config names
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
start(	  NumDaysInAMonth,
          NumHouseholdToFirmTradingRelations,
          NumConsecutiveMonthsWithAllPositionsFilledUpperLimit,
          WageGrowthRateUniformDistributionUpperSupport,
          InventoryUpperLimitRatio,
          InventoryLowerLimitRatio,
          PriceUpperLimitRatio,
          PriceLowerLimitRatio,
          ProbabilityOfSettingNewPrice,
          PriceGrowthRateUniformDistributionUpperSupport,
          ProbabilityOfHouseholdPickingNewProviderFirm,
          PriceThresholdOfHouseholdPickingNewProviderFirm,
          MaxNumberPotentialEmployersVisited,
          ProbabilityOfHouseholdVisitingPotentialNewEmployer,
          PlannedConsumptionIncreaseDecayingRate,
          MaxNumberProviderFirmsVisited,
          TechnologyProductivityParameter,
          ClaimedWageRatePercentageReductionIfUnemployed,
		  
		  FirmIds,
		  HouseholdIds
		  ) ->
	SimConfiguration =  #sim_config{days_in_one_month=NumDaysInAMonth,
		num_household_to_firm_trading_relations=NumHouseholdToFirmTradingRelations,
		num_consecutive_months_with_all_positions_filled_upper_limit=NumConsecutiveMonthsWithAllPositionsFilledUpperLimit,%gamma
		wage_growth_rate_uniform_distribution_upper_support=WageGrowthRateUniformDistributionUpperSupport,%delta
		inventory_upper_limit_ratio=InventoryUpperLimitRatio,%uphi_upper
		inventory_lower_limit_ratio=InventoryLowerLimitRatio,%uphi_lower
		price_upper_limit_ratio=PriceUpperLimitRatio,% lphi_upper
		price_lower_limit_ratio=PriceLowerLimitRatio,%lphi_lower
		probability_of_setting_new_price=ProbabilityOfSettingNewPrice,%theta
		price_growth_rate_uniform_distribution_upper_support=PriceGrowthRateUniformDistributionUpperSupport,%upsilon
		probability_of_household_picking_new_provider_firm=ProbabilityOfHouseholdPickingNewProviderFirm,
        price_threshold_of_household_picking_new_provider_firm=PriceThresholdOfHouseholdPickingNewProviderFirm,
		max_number_potential_employers_visited=MaxNumberPotentialEmployersVisited,
		probability_of_household_visiting_potential_new_employer=ProbabilityOfHouseholdVisitingPotentialNewEmployer,
		planned_consumption_increase_decaying_rate=PlannedConsumptionIncreaseDecayingRate,
		max_number_provider_firms_visited=MaxNumberProviderFirmsVisited,
		technology_productivity_parameter=TechnologyProductivityParameter,%lambda
		claimed_wage_rate_percentage_reduction_if_unemployed=ClaimedWageRatePercentageReductionIfUnemployed,
		
		firm_ids=FirmIds,
		household_ids=HouseholdIds		
    },
	household_fsm_m:start("HH1",1000, 2000, 200, 1, {1,2,3,4,5,6,7}, SimConfiguration),
	household_fsm_m:start("HH2",1500, 2500, 250, 2, {2,3,4,5,6,7,8}, SimConfiguration),
	firm_fsm_m:start("FI1",10000,100000, 1500, 50, 20, 3,SimConfiguration),
	firm_fsm_m:start("FI2",12000,120000, 1700, 70, 30, 3,SimConfiguration),
	gen_fsm:start({local, sim}, ?MODULE, {SimConfiguration}, []).
	
start_link(	  NumDaysInAMonth,
          NumHouseholdToFirmTradingRelations,
          NumConsecutiveMonthsWithAllPositionsFilledUpperLimit,
          WageGrowthRateUniformDistributionUpperSupport,
          InventoryUpperLimitRatio,
          InventoryLowerLimitRatio,
          PriceUpperLimitRatio,
          PriceLowerLimitRatio,
          ProbabilityOfSettingNewPrice,
          PriceGrowthRateUniformDistributionUpperSupport,
          ProbabilityOfHouseholdPickingNewProviderFirm,
          PriceThresholdOfHouseholdPickingNewProviderFirm,
          MaxNumberPotentialEmployersVisited,
          ProbabilityOfHouseholdVisitingPotentialNewEmployer,
          PlannedConsumptionIncreaseDecayingRate,
          MaxNumberProviderFirmsVisited,
          TechnologyProductivityParameter,
          ClaimedWageRatePercentageReductionIfUnemployed,
		  
		  FirmIds,
		  HouseholdIds
		  ) ->
	SimConfiguration =  #sim_config{days_in_one_month=NumDaysInAMonth,
		num_household_to_firm_trading_relations=NumHouseholdToFirmTradingRelations,
		num_consecutive_months_with_all_positions_filled_upper_limit=NumConsecutiveMonthsWithAllPositionsFilledUpperLimit,%gamma
		wage_growth_rate_uniform_distribution_upper_support=WageGrowthRateUniformDistributionUpperSupport,%delta
		inventory_upper_limit_ratio=InventoryUpperLimitRatio,%uphi_upper
		inventory_lower_limit_ratio=InventoryLowerLimitRatio,%uphi_lower
		price_upper_limit_ratio=PriceUpperLimitRatio,% lphi_upper
		price_lower_limit_ratio=PriceLowerLimitRatio,%lphi_lower
		probability_of_setting_new_price=ProbabilityOfSettingNewPrice,%theta
		price_growth_rate_uniform_distribution_upper_support=PriceGrowthRateUniformDistributionUpperSupport,%upsilon
		probability_of_household_picking_new_provider_firm=ProbabilityOfHouseholdPickingNewProviderFirm,
        price_threshold_of_household_picking_new_provider_firm=PriceThresholdOfHouseholdPickingNewProviderFirm,
		max_number_potential_employers_visited=MaxNumberPotentialEmployersVisited,
		probability_of_household_visiting_potential_new_employer=ProbabilityOfHouseholdVisitingPotentialNewEmployer,
		planned_consumption_increase_decaying_rate=PlannedConsumptionIncreaseDecayingRate,
		max_number_provider_firms_visited=MaxNumberProviderFirmsVisited,
		technology_productivity_parameter=TechnologyProductivityParameter,%lambda
		claimed_wage_rate_percentage_reduction_if_unemployed=ClaimedWageRatePercentageReductionIfUnemployed,
		
		firm_ids=FirmIds,
		household_ids=HouseholdIds		
    },		  
	household_fsm_m:start_link("HH1",1000, 2000, 200, 1, {1,2,3,4,5,6,7}, SimConfiguration),
	household_fsm_m:start_link("HH2",1500, 2500, 250, 1, {1,2,3,4,5,6,7}, SimConfiguration),
	firm_fsm_m:start_link("FI1",10000,100000, 1500, 50, 20, 3,SimConfiguration),
	firm_fsm_m:start_link("FI2",12000,120000, 1700, 70, 30, 3,SimConfiguration),
	gen_fsm:start_link({local, sim}, ?MODULE, {SimConfiguration}, []).

%%FSM PUBLIC API FUNCTIONS
new_step(StepNumber) ->
	io:format("Processing step number: ~w~n",[StepNumber]),
	gen_fsm:send_event(sim, {new_step, StepNumber}).

%GEN_FSM CALLBACKS
init(SimConfiguration) ->
	io:format("initialising SIM state~n"),
	{ok, normal, SimConfiguration, 0}.

normal(Event, State) ->
	io:format("Simulator sim_config is NORMAL. DaysInOneMonth: ~w~n",[State#sim_config.days_in_one_month]),
	case Event of
		{new_step, StepNumber} ->
			io:format("New simulation step: ~w~n",[StepNumber]),
			if 
				StepNumber rem State#sim_config.days_in_one_month == 1.0 ->
					MonthNumber= 1 + trunc(StepNumber / State#sim_config.days_in_one_month),
					io:format("This step is the first day of the month. Month number: ~w~n",[MonthNumber]),						
					household:first_day_of_month(MonthNumber),%also inject Sim_State
					firm:first_day_of_month(MonthNumber);%also inject Sim_State
				true ->
					io:format("This step is NOT the first day of the month.~n",[])
			end,
			if 
				StepNumber rem State#sim_config.days_in_one_month == 0.0 ->
					MonthNumber1= StepNumber / State#sim_config.days_in_one_month,
					io:format("This step is the last day of the month. Month number: ~w~n",[MonthNumber1]),					
					household_:last_day_of_month(MonthNumber1),%also inject Sim_State
					firm:last_day_of_month(MonthNumber1);%also inject Sim_State
				true ->
					io:format("This step is NOT the last day of the month.~n",[])
			end,			
			household:daily_step(StepNumber),%also inject Sim_State
			firm:daily_step(StepNumber),%also inject Sim_State
			%TODO:here query all firms and households to regenerate household and firm tuple list containing all firm-household relationships
			%then inject these into Sim state (sim_configuration record)
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