-module(enumeration_SUITE).
-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([start/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
	[start].

ets_owner() ->
    receive
         stop -> exit(normal)
    end.

init_per_suite(Config) ->
    Pid = spawn(fun ets_owner/0),
    TabId = ets:new(conns, [set, public, {heir, Pid, []}]),
    {ok, Journal} = journald_api:open(),
    ets:insert(TabId, {journal, Journal}), 
    [{table,TabId},{table_owner, Pid} | Config].

end_per_suite(Config) ->
    ?config(table_owner, Config) ! stop.

%This test will give you the last three entries of the journal,
%then the last one will be repeated two times (to test restart_data()).
start(Config) ->
    TabId = ?config(table, Config),
    [{journal, Journal}] = ets:lookup(TabId, journal),
    ok = tail(Journal, 3),
    ok = repeat_last(Journal, 2).

%get the fields of the last N entries
tail(J, N) ->
	ok = journald_api:seek_tail(J),
	ok = journald_api:previous(J),
	ok = skip_previous(J, N-1),
	ok = get_all_entries(J),
	ok.

%repeat fields of last entry N times
repeat_last(_,0) -> ok;
repeat_last(J,N) ->
	ok = journald_api:seek_tail(J),
	ok = journald_api:previous(J),
	ok = journald_api:restart_data(J),
	ok = get_all_fields(J),
	io:format("\n"),
	ok = repeat_last(J,N-1).


get_all_entries(J) ->
	ok = get_all_fields(J),
	io:format("\n"),
	case journald_api:next(J) of
		ok 			->	get_all_entries(J);
		eaddrnotavail	-> 	ok
	end.

skip_previous(_, 0) -> ok;
skip_previous(J, N) ->
	ok = journald_api:previous(J),
	skip_previous(J, N-1).

get_all_fields(J) ->
	case journald_api:enumerate_data(J) of
		{ok, Msg} 	-> 	io:format(Msg ++ "\n"),
						get_all_fields(J);
		eaddrnotavail	-> 	ok
	end.

