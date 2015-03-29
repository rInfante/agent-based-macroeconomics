-module(csv_file).

-export([write_header/2, write_data/2]).

write_header(Source, HeaderNames) ->
	FileName = get_file_name(Source),
	{ok, IO} = file:open(FileName, [write]),
	io:fwrite(IO, get_csv_format(HeaderNames), HeaderNames),
	file:close(IO).

write_data(Source, DataFields) ->
	FileName = get_file_name(Source),
	{ok, IO} = file:open(FileName, [append]),
	io:fwrite(IO, get_csv_format(DataFields), DataFields),
	file:close(IO).
	
get_file_name(Source) ->
	atom_to_list(Source) ++ ".txt".
	
get_csv_format(L) ->
	item_selection:truncate_last(lists:flatten(lists:map(
			fun(X)-> 	
				case is_list(X) of 
					true -> "~s,";
					false -> "~w,"
				end
			end, L))) ++ "~n".
	