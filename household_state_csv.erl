-module(household_state_csv).

-export([write_header/0, write_data/3]).

write_header()->
	HeaderAtoms = [step_number, step_type] ++ household_state:get_all_fields(),
	HeaderNames = lists:map(fun(A)->atom_to_list(A) end, HeaderAtoms),
	csv_file:write_header(household, HeaderNames). 
	
write_data(StepNumber, StepType, FirmState) -> 
	DataFields = [StepNumber, atom_to_list(StepType)] ++ household_state:get_all_values(FirmState),
	csv_file:write_data(household, DataFields).