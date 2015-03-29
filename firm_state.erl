-module(firm_state).

-export([get_value/2, get_values/2, firm_id_to_str/1, firm_id_to_atom/1, get_all_fields/0, get_all_values/1]).

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
get_value(dash_delimited_employee_ids, FirmState) ->
	EmployeeIds = FirmState#firm_state.employee_ids,
	item_selection:dash_delimited_integer_list(EmployeeIds);	
get_value(firm_id_as_atom, FirmState) ->
	FirmId = FirmState#firm_state.firm_id,	
	firm_id_to_atom(FirmId).
	
get_all_fields()->
	[
		firm_id, inventory_f, liquidity_f, wage_rate_f, price_f, num_work_positions_available, num_work_positions_filled,
		work_position_has_been_offered, work_position_has_been_accepted, num_consecutive_months_all_work_positions_filled,
		fired_employee_id, 
		monthly_demand_of_consumption_goods, monthly_marginal_costs, dash_delimited_employee_ids
	].

%%TODO: This can be generalised	
get_values(Args, FirmState) ->
	[get_value(Arg, FirmState) || Arg <- Args].
	
get_all_values(FirmState)->
	get_values(get_all_fields(), FirmState).

firm_id_to_str(FirmId) ->
	string:concat("FI", integer_to_list(FirmId)).
	
firm_id_to_atom(FirmId) ->
	FirmIdStr = firm_id_to_str(FirmId),
	list_to_atom(FirmIdStr).
	
