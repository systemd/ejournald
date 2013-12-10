-module(journald_api_read_SUITE).

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, ets_owner/0]).

-export([just_get_data/1, next_entry/1, previous_entry/1, add_match/1, flush_matches/1,
        add_conjunction/1, add_disjunction/1, seek_head/1, seek_tail/1,
        cursor_test/1, is_right_cursor/2, stress1/1, stress2/1, receiver/0]). 

-export([send_test/1]).

-include_lib("common_test/include/ct.hrl").

%%%The journal is opened and closed just _one_ time.
%%%Every command except the open() and close() command of the interface will be used in a random order.

all() ->
    [{group, session},{group, stress},cursor_test]. 

groups()    ->  [{session, [shuffle, sequence, {repeat, 5}], 
                [just_get_data, next_entry, previous_entry, add_match, flush_matches,
                add_conjunction, add_disjunction, seek_head, seek_tail]},
                {stress, [], [stress1, stress2]}].

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

%%%Saschas tests
%%%RANDOM API TEST
just_get_data(Config) ->
    TabId = ?config(table, Config),
    [{journal, Journal}] = ets:lookup(TabId, journal),
    List = ["MESSAGE", "PRIORITY", "_PID"],
    [Elem|_] = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List])],
    case    journald_api:get_data(Journal, Elem) of
        {ok, _Msg}     ->     ok;
        {error, Msg}     ->     io:format(Msg)
    end.

next_entry(Config) ->
    TabId = ?config(table, Config),
    [{journal, Journal}] = ets:lookup(TabId, journal),
    case journald_api:next(Journal) of 
        ok             -> ok;
        eaddrnotavail  -> ok;
        {error, Msg}     -> io:format(Msg)
    end.

previous_entry(Config) ->
    TabId = ?config(table, Config),
    [{journal, Journal}] = ets:lookup(TabId, journal),
    case journald_api:previous(Journal) of
        ok            -> ok;
        eaddrnotavail -> ok;
        {error, Msg}     -> io:format(Msg)
    end.

add_match(Config) ->
    TabId = ?config(table, Config),
    [{journal, Journal}] = ets:lookup(TabId, journal),
    List = ["PRIORITY=1", "PRIORITY=2", "PRIORITY=5", "_PID"],
    [Elem|_] = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List])],
    case journald_api:add_match(Journal, Elem) of
        ok            -> ok;
        {error, Msg}     -> io:format(Msg)
    end.

flush_matches(Config) ->
    TabId = ?config(table, Config),
    [{journal, Journal}] = ets:lookup(TabId, journal),
    ok = journald_api:flush_matches(Journal).

add_conjunction(Config) ->
    TabId = ?config(table, Config),
    [{journal, Journal}] = ets:lookup(TabId, journal),
    case journald_api:add_conjunction(Journal) of
        ok            -> ok;
        {error, Msg}     -> io:format(Msg)
    end.

add_disjunction(Config) ->
    TabId = ?config(table, Config),
    [{journal, Journal}] = ets:lookup(TabId, journal),
    case journald_api:add_disjunction(Journal) of
        ok            -> ok;
        {error, Msg}     -> io:format(Msg)
    end.

seek_head(Config) ->
    TabId = ?config(table, Config),
    [{journal, Journal}] = ets:lookup(TabId, journal),
    case journald_api:seek_head(Journal) of
        ok            -> ok;
        {error, Msg}     -> io:format(Msg)
    end.

seek_tail(Config) ->
    TabId = ?config(table, Config),
    [{journal, Journal}] = ets:lookup(TabId, journal),
    case journald_api:seek_tail(Journal) of
        ok            -> ok;
        {error, Msg}     -> io:format(Msg)
    end.

cursor_test(Config) ->
    TabId = ?config(table, Config),
    [{journal, Journal}] = ets:lookup(TabId, journal),
    case journald_api:get_cursor(Journal) of
        {ok, Cursor}    -> is_right_cursor(Journal, Cursor);
        {error, Msg}     -> io:format(Msg)
    end.
%%RANDOM API TEST

is_right_cursor(Journal, Cursor) ->
    is_atom(journald_api:next(Journal)),
    eaddrnotavail = journald_api:test_cursor(Journal, Cursor),
    journald_api:previous(Journal),
    ok = journald_api:test_cursor(Journal, Cursor),
    ok = journald_api:next(Journal),
    ok = journald_api:seek_cursor(Journal, Cursor),
    ok = journald_api:next(Journal),
    ok = journald_api:test_cursor(Journal, Cursor).
    

%write and read parallel    
stress1(_) ->               
    receive
        after(500) -> ok
    end,
    rpc:pmap({?MODULE, send_test},[],lists:seq(1,1000)),
    {node(), nodes()}.

send_test(V1) ->
   [journald_api:sendv([{"MESSAGE=", "iolist" ++ integer_to_list(V2) ++ "/" ++ integer_to_list(V1)}]) || V2 <- lists:seq(1,100)].

stress2(Config) ->               
    TabId = ?config(table, Config),
    [{journal, Journal}] = ets:lookup(TabId, journal),
    Pid = spawn(?MODULE, receiver, []),
    journald_api:open_notifier(Journal, Pid).

receiver() ->
    receive
        ok -> 
            nop,
            receiver()
    end.
