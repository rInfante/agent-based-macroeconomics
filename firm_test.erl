-module(firm_test).

-export([test/0]).

-include_lib("record_defs.hrl").

test() ->
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
		employee_ids=[1,5]
	},
	firm:start(Firm1).