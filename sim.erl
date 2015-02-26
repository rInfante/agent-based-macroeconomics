-module(sim).
-behavior(gen_fsm).

% public API
-export([start/3, start_link/3, new_step/1]).

%% gen_fsm callbacks
-export([init/1,
handle_event/3, handle_sync_event/4, handle_info/3,terminate/3, code_change/4,
 %custom sim_state names
normal/2]).

-include_lib("record_defs.hrl").

%%% PUBLIC API
start(SimState, Firms, Households) ->	
	lists:foreach(fun(F)-> firm:start(F) end, Firms),
	lists:foreach(fun(H)-> household:start(H) end, Households),
	gen_fsm:start({local, sim}, ?MODULE, SimState, []).
	
start_link(SimState, Firms, Households) ->	  
	lists:foreach(fun(F)-> firm:start(F) end, Firms),
	lists:foreach(fun(H)-> household:start(H) end, Households),
	gen_fsm:start_link({local, sim}, ?MODULE, SimState, []).

%%FSM PUBLIC API FUNCTIONS
new_step(StepNumber) ->
	io:format("Processing step number: ~w~n",[StepNumber]),
	gen_fsm:send_event(sim, {new_step, StepNumber}).

%GEN_FSM CALLBACKS
init(SimState) ->
	io:format("initialising SIM state~n"),
	{ok, normal, SimState, 0}.

normal(Event, State) ->
	io:format("State = ~w~n", [State]),		
	case Event of
		{new_step, StepNumber} ->
			DaysInOneMonth = sim_state:get_value(days_in_one_month, State),
			HouseholdIds = sim_state:get_value(household_ids, State),
			FirmIds = sim_state:get_value(firm_ids, State),
			io:format("New simulation step: ~w~n",[StepNumber]),
			case is_first_day_of_month(StepNumber, DaysInOneMonth) of 
				true ->
					MonthNumber = 1 + (StepNumber div DaysInOneMonth),
					io:format("This step is the first day of the month. Month number: ~w~n",[MonthNumber]),
					lists:foreach(fun(Id)->firm:first_day_of_month(Id, MonthNumber, State) end, FirmIds),				
					lists:foreach(fun(Id)->household:first_day_of_month(Id, MonthNumber, State) end, HouseholdIds);
				false ->
					io:format("This step is NOT the first day of the month.~n",[])
			end,
			case is_first_last_of_month(StepNumber, DaysInOneMonth) of 
				true ->
					MonthNumber1 = StepNumber div DaysInOneMonth,
					io:format("This step is the last day of the month. Month number: ~w~n",[MonthNumber1]),		
					lists:foreach(fun(Id)->household:last_day_of_month(Id, MonthNumber1, State) end, HouseholdIds),
					lists:foreach(fun(Id)->firm:last_day_of_month(Id, MonthNumber1, State) end, FirmIds); 						
				false ->
					io:format("This step is NOT the last day of the month.~n",[])
			end,
			lists:foreach(fun(Id)->household:daily_step(Id, StepNumber, State) end, HouseholdIds), 
			lists:foreach(fun(Id)->firm:daily_step(Id, StepNumber, State) end, FirmIds), 
			%TODO:here query all firms and households to regenerate household and firm tuple list containing all firm-household relationships
			%then inject these into Sim state (sim_state record)
			io:format("Simulation step: ~w COMPLETED~n",[StepNumber]),
			{next_state, normal, State, 10000};
		timeout ->
			io:format("Nothing has happened in the simulator...~n"),
			{next_state, normal, State, 10000};
		_ ->
			io:format("Unknown event. Staying normal.~n"),
			{next_state, normal, State, 1000}
	end.

%%GEN FSM BEHAVIOUR TEMPLATE
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
	
%PRIVATE	
is_first_day_of_month(StepNumber, DaysInOneMonth) -> 
	StepNumber rem DaysInOneMonth == 1.0.
is_first_last_of_month(StepNumber, DaysInOneMonth) -> 
	StepNumber rem DaysInOneMonth == 0.0.	