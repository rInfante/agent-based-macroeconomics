-module(firm).
-behavior(gen_fsm).

% public API
-export([start/1, start_link/1, daily_step/2, first_day_of_month/2, last_day_of_month/2]).

%debug
-export([firm_id_to_str/1, firm_id_to_atom/1]).

%% gen_fsm callbacks
-export([init/1,
handle_event/3, handle_sync_event/4, handle_info/3,terminate/3, code_change/4,
 %cusatom state names
normal/2]).

-include_lib("record_defs.hrl").	

%%% PUBLIC API
start(FirmState) ->		
	gen_fsm:start({local, firm_id_to_atom(FirmState#firm_state.firm_id)}, ?MODULE, FirmState, []).
start_link(FirmState ) ->
	gen_fsm:start_link({local, firm_id_to_atom(FirmState#firm_state.firm_id)}, ?MODULE, FirmState, []).

%%FSM PUBLIC API FUNCTIONS
daily_step(FirmId, DayNumber) ->
	io:format("Processing day ~w for firm id:~w~n",[DayNumber, FirmId]),
	increase_inventory(FirmId).

first_day_of_month(FirmId, MonthNumber) ->
	io:format("Processing first day of month: ~w for firm id:~w~n",[MonthNumber, FirmId]).
last_day_of_month(FirmId, MonthNumber) ->
	io:format("Processing last day of month: ~w for firm id:~w~n",[MonthNumber, FirmId]).

%Private functions
increase_inventory(FirmId) ->
	io:format("Increasing inventory_f of instance ~w. ~n",[FirmId]),
	gen_fsm:send_event(firm_id_to_atom(FirmId), increase_inventory).

%GEN_FSM CALLBACKS
init(State) ->
	io:format("FIRM_FSM initialising state with Id:~w, Inventory:~w, Liquidity:~w, WageRate: ~w, Price: ~w, Number of Labourers:~w, , EmployeeIds: ~w~n",[State#firm_state.firm_id, State#firm_state.inventory_f, State#firm_state.liquidity_f, State#firm_state.wage_rate_f, State#firm_state.price_f, State#firm_state.num_work_positions_available, State#firm_state.employee_ids]),
	{ok, normal, State, 2000}.

normal(Event, State) ->
	io:format("Firm ~w state is NORMAL. Inventory:~w, Liquidity:~w, WageRate:~w, Price:~w, Number of Labourers:~w, Employee Ids:~w~n",[State#firm_state.firm_id, State#firm_state.inventory_f, State#firm_state.liquidity_f, State#firm_state.wage_rate_f, State#firm_state.price_f, State#firm_state.num_work_positions_available, State#firm_state.employee_ids]),
	case Event of
		increase_inventory ->
			NewInventory = State#firm_state.inventory_f + State#firm_state.num_work_positions_available * State#firm_state.sim_configuration#sim_config.technology_productivity_parameter,
			io:format("Firm id:~w is increasing inventory_f from ~w to ~w~n",[State#firm_state.firm_id,State#firm_state.inventory_f, NewInventory]),			
			{next_state, normal, State#firm_state{inventory_f=NewInventory}, 10000};
		timeout ->
			io:format("Nothing has happened to NORMAL firm id:~w...~n",[State#firm_state.firm_id]),
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
	
%%PRIVATE
firm_id_to_str(FirmId) ->
	string:concat("FI", integer_to_list(FirmId)).
	
firm_id_to_atom(FirmId) ->
	FirmIdStr = firm_id_to_str(FirmId),
	list_to_atom(FirmIdStr).







	

