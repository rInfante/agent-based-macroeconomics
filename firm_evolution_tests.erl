-module(firm_evolution_tests).
-include_lib("eunit/include/eunit.hrl").

-include_lib("record_defs.hrl").

evolve_num_consecutive_months_with_all_positions_filled_test_()->
	[
		?_assert(firm_evolution:evolve_num_consecutive_months_with_all_positions_filled(#firm_state
			{
				num_work_positions_available=10, 
				num_work_positions_filled=1,
				num_consecutive_months_all_work_positions_filled=0
			}) =:= 0),
		?_assert(firm_evolution:evolve_num_consecutive_months_with_all_positions_filled(#firm_state
			{
				num_work_positions_available=1, 
				num_work_positions_filled=1,
				num_consecutive_months_all_work_positions_filled=0
			}) =:= 1),
		?_assert(firm_evolution:evolve_num_consecutive_months_with_all_positions_filled(#firm_state
			{
				num_work_positions_available=1, 
				num_work_positions_filled=1,
				num_consecutive_months_all_work_positions_filled=1
			}) =:= 2),
		?_assert(firm_evolution:evolve_num_consecutive_months_with_all_positions_filled(#firm_state
			{
				num_work_positions_available=10, 
				num_work_positions_filled=10,
				num_consecutive_months_all_work_positions_filled=10
			}) =:= 11)			
	].
	
evolve_wage_rate_test_() ->
	[
		?_assert(firm_evolution:evolve_wage_rate(#firm_state
			{	
				num_work_positions_available = 10, 
				num_work_positions_filled = 1,
				num_consecutive_months_all_work_positions_filled = 0,
				
				work_position_has_been_offered = true, 		%TESTING THIS CASE
				work_position_has_been_accepted = false,	%TESTING THIS CASE
				
				wage_rate_f = 1000,
				
				sim_configuration= #sim_config
									{
										wage_growth_rate_uniform_distribution_upper_support=0.019 %delta
									}
			}) > 1000), % TESTING WAGE RATE INCREASE
		?_assert(firm_evolution:evolve_wage_rate(#firm_state
			{	
				num_work_positions_available = 10, 
				num_work_positions_filled = 10,
				num_consecutive_months_all_work_positions_filled = 2,%TESTING THIS CASE
				
				work_position_has_been_offered = false, 		
				work_position_has_been_accepted = false,	
				
				wage_rate_f = 1000,
				
				sim_configuration= #sim_config
									{
										wage_growth_rate_uniform_distribution_upper_support=0.019 %delta
									}
			}) < 1000), % TESTING WAGE RATE DECREASE
		?_assert(firm_evolution:evolve_wage_rate(#firm_state
			{	
				num_work_positions_available = 10, 
				num_work_positions_filled = 1,
				num_consecutive_months_all_work_positions_filled = 0,
				
				work_position_has_been_offered = true, 		%TESTING THIS CASE
				work_position_has_been_accepted = true,	
				
				wage_rate_f = 1000,
				
				sim_configuration= #sim_config
									{
										wage_growth_rate_uniform_distribution_upper_support=0.019 %delta
									}
			}) < 1000), % TESTING WAGE DECREASE (due to false work_position_has_been_offered)	
		?_assert(firm_evolution:evolve_wage_rate(#firm_state
			{	
				num_work_positions_available = 10, 
				num_work_positions_filled = 9,							
				num_consecutive_months_all_work_positions_filled = 3, %TESTING num_consecutive_months_all_work_positions_filled going to 0
				
				work_position_has_been_offered = false, 		%TESTING THIS CASE
				work_position_has_been_accepted = false,	
				
				wage_rate_f = 1000,
				
				sim_configuration= #sim_config
									{
										wage_growth_rate_uniform_distribution_upper_support=0.019 %delta
									}
			}) =:= 1000) % TESTING WAGE UNCHANGED - case 1 (due to false work_position_has_been_offered)				
	].
	