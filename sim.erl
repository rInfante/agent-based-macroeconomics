-module(sim).
-behavior(gen_fsm).

% public API
-export([start/1, start_link/1, new_step/1]).

%% gen_fsm callbacks
-export([init/1,
handle_event/3, handle_sync_event/4, handle_info/3,terminate/3, code_change/4,
 %cusatom sim_config names
normal/2]).

-include_lib("record_defs.hrl").

%%% PUBLIC API
start(SimConfiguration) ->	
	household:start("HH1",1000, 2000, 200, 1, {1,2,3,4,5,6,7}, SimConfiguration),
	household:start("HH2",1500, 2500, 250, 2, {2,3,4,5,6,7,8}, SimConfiguration),
	firm:start("FI1",10000,100000, 1500, 50, 20, 3,SimConfiguration),
	firm:start("FI2",12000,120000, 1700, 70, 30, 3,SimConfiguration),
	gen_fsm:start({local, sim}, ?MODULE, {SimConfiguration}, []).
	
start_link(SimConfiguration) ->	  
	household:start_link("HH1",1000, 2000, 200, 1, {1,2,3,4,5,6,7}, SimConfiguration),
	household:start_link("HH2",1500, 2500, 250, 1, {1,2,3,4,5,6,7}, SimConfiguration),
	firm:start_link("FI1",10000,100000, 1500, 50, 20, 3,SimConfiguration),
	firm:start_link("FI2",12000,120000, 1700, 70, 30, 3,SimConfiguration),
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