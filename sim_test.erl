-module(sim_test).

-export([test/0]).

-include_lib("record_defs.hrl").

test() ->
	SimConfiguration = #sim_config
	{
		days_in_one_month=21,
		num_household_to_firm_trading_relations=7,
		num_consecutive_months_with_all_positions_filled_upper_limit=24,%gamma
		wage_growth_rate_uniform_distribution_upper_support=0.019,%delta
		inventory_upper_limit_ratio=1.0,%uphi_upper
		inventory_lower_limit_ratio=0.25,%uphi_lower
		price_upper_limit_ratio=1.15,% lphi_upper
		price_lower_limit_ratio=1.025,%lphi_lower
		probability_of_setting_new_price=0.75,%theta
		price_growth_rate_uniform_distribution_upper_support=0.02,%upsilon
		probability_of_household_picking_new_provider_firm=0.25,
        price_threshold_of_household_picking_new_provider_firm=0.01,
		max_number_potential_employers_visited=5,
		probability_of_household_visiting_potential_new_employer=0.1,
		planned_consumption_increase_decaying_rate=0.75,
		max_number_provider_firms_visited=7,
		technology_productivity_parameter=3.0,%lambda
		claimed_wage_rate_percentage_reduction_if_unemployed=0.10,

		firm_employees_lookup=[{1, [1,5]}, {2, [2,6]}, {3,[3,7]}, {4,[4,8]}, {5,[9,13]}, {6,[10,14]}, {7,[11,15]}, {8,[12,16]}],
		household_ids=[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
    },
	
	Firm1 = #firm_state
	{
		firm_id=1, 
		inventory_f=1000, %i_f
		liquidity_f=100000, %m_f
		wage_rate_f=1000, %w_f
		price_f=200, %p_f
		num_work_positions_available=10, 
		num_work_positions_filled=1,
		work_position_has_been_offered=false,
		work_position_has_been_accepted=false,
		num_consecutive_months_all_work_positions_filled=0,
		fired_employee_id=0,
		monthly_demand_of_consumption_goods=0,
		monthly_marginal_costs=0,		
		employee_ids=[1,5],
		
		sim_configuration=SimConfiguration
	},
	Firm2 = #firm_state
	{
		firm_id=2, 
		inventory_f=1000, %i_f
		liquidity_f=100000, %m_f
		wage_rate_f=1000, %w_f
		price_f=80, %p_f
		num_work_positions_available=10, 
		num_work_positions_filled=1,
		work_position_has_been_offered=false,
		work_position_has_been_accepted=false,
		num_consecutive_months_all_work_positions_filled=0,
		fired_employee_id=0,
		monthly_demand_of_consumption_goods=0,
		monthly_marginal_costs=0,		
		employee_ids=[2,6],
		
		sim_configuration=SimConfiguration
	},	
	Firm3 = #firm_state
	{
		firm_id=3, 
		inventory_f=100000, %i_f
		liquidity_f=100000, %m_f
		wage_rate_f=1000, %w_f
		price_f=5, %p_f
		num_work_positions_available=10, 
		num_work_positions_filled=1,
		work_position_has_been_offered=false,
		work_position_has_been_accepted=false,
		num_consecutive_months_all_work_positions_filled=0,
		fired_employee_id=0,
		monthly_demand_of_consumption_goods=0,
		monthly_marginal_costs=0,		
		employee_ids=[3,7],
		
		sim_configuration=SimConfiguration
	},
	Firm4 = #firm_state
	{
		firm_id=4, 
		inventory_f=1000, %i_f
		liquidity_f=100000, %m_f
		wage_rate_f=1000, %w_f
		price_f=20, %p_f
		num_work_positions_available=10, 
		num_work_positions_filled=1,
		work_position_has_been_offered=false,
		work_position_has_been_accepted=false,
		num_consecutive_months_all_work_positions_filled=0,
		fired_employee_id=0,
		monthly_demand_of_consumption_goods=0,
		monthly_marginal_costs=0,		
		employee_ids=[4,8],
		
		sim_configuration=SimConfiguration
	},
	Firm5 = #firm_state
	{
		firm_id=5, 
		inventory_f=10000, %i_f
		liquidity_f=100000, %m_f
		wage_rate_f=1000, %w_f
		price_f=30, %p_f
		num_work_positions_available=10, 
		num_work_positions_filled=1,
		work_position_has_been_offered=false,
		work_position_has_been_accepted=false,
		num_consecutive_months_all_work_positions_filled=0,
		fired_employee_id=0,
		monthly_demand_of_consumption_goods=0,
		monthly_marginal_costs=0,		
		employee_ids=[9,13],
		
		sim_configuration=SimConfiguration
	},	
	Firm6 = #firm_state
	{
		firm_id=6, 
		inventory_f=10000, %i_f
		liquidity_f=100000, %m_f
		wage_rate_f=1000, %w_f
		price_f=40, %p_f
		num_work_positions_available=10, 
		num_work_positions_filled=1,
		work_position_has_been_offered=false,
		work_position_has_been_accepted=false,
		num_consecutive_months_all_work_positions_filled=0,
		fired_employee_id=0,
		monthly_demand_of_consumption_goods=0,
		monthly_marginal_costs=0,		
		employee_ids=[10,14],
		
		sim_configuration=SimConfiguration
	},	
	Firm7 = #firm_state
	{
		firm_id=7, 
		inventory_f=10000, %i_f
		liquidity_f=100000, %m_f
		wage_rate_f=1000, %w_f
		price_f=50, %p_f
		num_work_positions_available=10, 
		num_work_positions_filled=1,
		work_position_has_been_offered=false,
		work_position_has_been_accepted=false,
		num_consecutive_months_all_work_positions_filled=0,
		fired_employee_id=0,
		monthly_demand_of_consumption_goods=0,
		monthly_marginal_costs=0,		
		employee_ids=[11,15],
		
		sim_configuration=SimConfiguration
	},	
	Firm8 = #firm_state
	{
		firm_id=8, 
		inventory_f=10000, %i_f
		liquidity_f=100000, %m_f
		wage_rate_f=1000, %w_f
		price_f=10, %p_f
		num_work_positions_available=10, 
		num_work_positions_filled=1,
		work_position_has_been_offered=false,
		work_position_has_been_accepted=false,
		num_consecutive_months_all_work_positions_filled=0,
		fired_employee_id=0,
		monthly_demand_of_consumption_goods=0,
		monthly_marginal_costs=0,		
		employee_ids=[12,16],
		
		sim_configuration=SimConfiguration
	},
	
	Household1=#household_state 
	{
		household_id=1, 
		reservation_wage_rate_h=800, %w_h
		liquidity_h=5000, %m_h
		planned_monthly_consumption_expenditure=300, %c_r_h
		provider_firms_ids=[1,2,3,4], %type A firms
		employer_firm_id=1, % type B firm
		
		sim_configuration=SimConfiguration		
	},
	Household2=#household_state 
	{
		household_id=2, 
		reservation_wage_rate_h=800, %w_h
		liquidity_h=5000, %m_h
		planned_monthly_consumption_expenditure=300, %c_r_h
		provider_firms_ids=[2,3,4,5], %type A firms
		employer_firm_id=2, % type B firm
		
		sim_configuration=SimConfiguration		
	},
	Household3=#household_state 
	{
		household_id=3, 
		reservation_wage_rate_h=800, %w_h
		liquidity_h=5000, %m_h
		planned_monthly_consumption_expenditure=300, %c_r_h
		provider_firms_ids=[3,4,5,6], %type A firms
		employer_firm_id=3, % type B firm
		
		sim_configuration=SimConfiguration		
	},
	Household4=#household_state 
	{
		household_id=4, 
		reservation_wage_rate_h=800, %w_h
		liquidity_h=5000, %m_h
		planned_monthly_consumption_expenditure=300, %c_r_h
		provider_firms_ids=[5,6,7,8], %type A firms
		employer_firm_id=4, % type B firm
		
		sim_configuration=SimConfiguration		
	},
	Household5=#household_state 
	{
		household_id=5, 
		reservation_wage_rate_h=800, %w_h
		liquidity_h=5000, %m_h
		planned_monthly_consumption_expenditure=300, %c_r_h
		provider_firms_ids=[1,2,3,4], %type A firms
		employer_firm_id=1, % type B firm
		
		sim_configuration=SimConfiguration		
	},
	Household6=#household_state 
	{
		household_id=6, 
		reservation_wage_rate_h=800, %w_h
		liquidity_h=5000, %m_h
		planned_monthly_consumption_expenditure=300, %c_r_h
		provider_firms_ids=[2,3,4,5], %type A firms
		employer_firm_id=2, % type B firm
		
		sim_configuration=SimConfiguration		
	},
	Household7=#household_state 
	{
		household_id=7, 
		reservation_wage_rate_h=800, %w_h
		liquidity_h=5000, %m_h
		planned_monthly_consumption_expenditure=300, %c_r_h
		provider_firms_ids=[3,4,5,6], %type A firms
		employer_firm_id=3, % type B firm
		
		sim_configuration=SimConfiguration		
	},
	Household8=#household_state 
	{
		household_id=8, 
		reservation_wage_rate_h=800, %w_h
		liquidity_h=5000, %m_h
		planned_monthly_consumption_expenditure=300, %c_r_h
		provider_firms_ids=[5,6,7,8], %type A firms
		employer_firm_id=4, % type B firm
		
		sim_configuration=SimConfiguration		
	},	
	Household9=#household_state 
	{
		household_id=9, 
		reservation_wage_rate_h=800, %w_h
		liquidity_h=5000, %m_h
		planned_monthly_consumption_expenditure=300, %c_r_h
		provider_firms_ids=[1,2,3,4], %type A firms
		employer_firm_id=5, % type B firm
		
		sim_configuration=SimConfiguration		
	},
	Household10=#household_state 
	{
		household_id=10, 
		reservation_wage_rate_h=800, %w_h
		liquidity_h=5000, %m_h
		planned_monthly_consumption_expenditure=300, %c_r_h
		provider_firms_ids=[2,3,4,5], %type A firms
		employer_firm_id=6, % type B firm
		
		sim_configuration=SimConfiguration		
	},
	Household11=#household_state 
	{
		household_id=11, 
		reservation_wage_rate_h=800, %w_h
		liquidity_h=5000, %m_h
		planned_monthly_consumption_expenditure=300, %c_r_h
		provider_firms_ids=[3,4,5,6], %type A firms
		employer_firm_id=7, % type B firm
		
		sim_configuration=SimConfiguration		
	},
	Household12=#household_state 
	{
		household_id=12, 
		reservation_wage_rate_h=800, %w_h
		liquidity_h=5000, %m_h
		planned_monthly_consumption_expenditure=300, %c_r_h
		provider_firms_ids=[5,6,7,8], %type A firms
		employer_firm_id=8, % type B firm
		
		sim_configuration=SimConfiguration		
	},
	Household13=#household_state 
	{
		household_id=13, 
		reservation_wage_rate_h=800, %w_h
		liquidity_h=5000, %m_h
		planned_monthly_consumption_expenditure=300, %c_r_h
		provider_firms_ids=[1,2,3,4], %type A firms
		employer_firm_id=5, % type B firm
		
		sim_configuration=SimConfiguration		
	},
	Household14=#household_state 
	{
		household_id=14, 
		reservation_wage_rate_h=800, %w_h
		liquidity_h=5000, %m_h
		planned_monthly_consumption_expenditure=300, %c_r_h
		provider_firms_ids=[2,3,4,5], %type A firms
		employer_firm_id=5, % type B firm
		
		sim_configuration=SimConfiguration		
	},
	Household15=#household_state 
	{
		household_id=15, 
		reservation_wage_rate_h=800, %w_h
		liquidity_h=5000, %m_h
		planned_monthly_consumption_expenditure=300, %c_r_h
		provider_firms_ids=[3,4,5,6], %type A firms
		employer_firm_id=7, % type B firm
		
		sim_configuration=SimConfiguration		
	},
	Household16=#household_state 
	{
		household_id=16, 
		reservation_wage_rate_h=800, %w_h
		liquidity_h=5000, %m_h
		planned_monthly_consumption_expenditure=300, %c_r_h
		provider_firms_ids=[5,6,7,8], %type A firms
		employer_firm_id=8, % type B firm
		
		sim_configuration=SimConfiguration		
	},		
	
	%%KICK SIMULATION!
	sim:start(SimConfiguration,
		[Firm1, Firm2, Firm3, Firm4, Firm5, Firm6, Firm7, Firm8], 
		[Household1, Household2, Household3, Household4,
			Household5, Household6, Household7, Household8,
			Household9, Household10, Household11, Household12,
			Household13, Household14, Household15, Household16]).	