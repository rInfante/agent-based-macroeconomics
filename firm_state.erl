-module(firm_state).

-export([get_value/2, get_values/2, firm_id_to_str/1, firm_id_to_atom/1]).

-include_lib("record_defs.hrl").

get_value(firm_id, FirmState) ->
	FirmState#firm_state.firm_id;
get_value(inventory_f, FirmState) ->
	FirmState#firm_state.inventory_f;
get_value(liquidity_f, FirmState) ->
	FirmState#firm_state.liquidity_f;
get_value(wage_rate_f, FirmState) ->
	FirmState#firm_state.wage_rate_f;
get_value(price_f, FirmState) ->
	FirmState#firm_state.price_f;
get_value(num_work_positions_available, FirmState) ->
	FirmState#firm_state.num_work_positions_available;
get_value(num_work_positions_filled, FirmState) ->
	FirmState#firm_state.num_work_positions_filled;
get_value(work_position_has_been_offered, FirmState) ->
	FirmState#firm_state.work_position_has_been_offered;	
get_value(work_position_has_been_accepted, FirmState) ->
	FirmState#firm_state.work_position_has_been_accepted;
get_value(num_consecutive_months_all_work_positions_filled, FirmState) ->
	FirmState#firm_state.num_consecutive_months_all_work_positions_filled;
get_value(fired_employee_id, FirmState) ->
	FirmState#firm_state.fired_employee_id;
get_value(monthly_demand_of_consumption_goods, FirmState) ->
	FirmState#firm_state.monthly_demand_of_consumption_goods;
get_value(monthly_marginal_costs, FirmState) ->
	FirmState#firm_state.monthly_marginal_costs;
get_value(employee_ids, FirmState) ->
	FirmState#firm_state.employee_ids;
get_value(firm_id_as_atom, FirmState) ->
	FirmId = FirmState#firm_state.firm_id,	
	firm_id_to_atom(FirmId).

%%TODO: This can be generalised	
get_values(Args, FirmState) ->
	[get_value(Arg, FirmState) || Arg <- Args].

firm_id_to_str(FirmId) ->
	string:concat("FI", integer_to_list(FirmId)).
	
firm_id_to_atom(FirmId) ->
	FirmIdStr = firm_id_to_str(FirmId),
	list_to_atom(FirmIdStr).
	
