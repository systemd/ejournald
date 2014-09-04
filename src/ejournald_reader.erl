-module(ejournald_reader).
-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-record(time_frame, {
	fst_cursor,
	snd_cursor
}).
-record(notifier, {
	active,
	user_pids
}).
-record(state, {
	fd, 
	time_frame,
	direction,
	notifier
}).

%% ----------------------------------------------------------------------------------------------------
%% -- gen_server callbacks
start_link(Options) ->
	gen_server:start_link(?MODULE, [Options], []).

init(Options) ->
	evaluate_options(Options).

handle_call({evaluate, Options}, _From, State) ->
	{Result, NewState} = evaluate_log_options(Options, State),
	{reply, Result, NewState};
handle_call({register_notifier, Pid}, _From, State) ->
	{reply, ok, register_notifier(Pid, State)};
handle_call({unregister_notifier, Pid}, _From, State) ->
	{reply, ok, unregister_notifier(Pid, State)};
handle_call(last_cursor, _From, State) ->
	Cursor = get_last_cursor(State),
	{reply, Cursor, State};
handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_info(journal_append, State = #state{notifier = Notifier}) -> 
	UserPids = Notifier#notifier.user_pids,
	[ Pid ! journal_append || Pid <- UserPids ],
	{noreply, State};
handle_info(_Msg, State) -> 
	{noreply, State}.

%% unused
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_,_,State) -> {ok, State}. 

%% ----------------------------------------------------------------------------------------------------
%% -- helpers
evaluate_options(Options) ->
	{ok, Fd} = journald_api:open(),
	Dir = proplists:get_value(direction, Options, top),
	Notifier = #notifier{active = false, user_pids = []},
    State = #state{fd = Fd, direction = Dir, time_frame = #time_frame{}, notifier = Notifier},
   	{ok, State}.

reset_entry(Pos, State) ->
	move(Pos, State).

next_entry(State = #state{fd = Fd}) ->
	case reset_entry(next, State) of
		ok -> get_fields(Fd);
		Error -> Error
	end.

get_fields(Fd) ->
	get_fields(Fd, []).

get_fields(Fd, Akk) ->
	case journald_api:enumerate_data(Fd) of
		{ok, Data} ->
			get_fields(Fd, Akk ++ [Data]);
		_ ->
			Akk
	end. 

next_field(FieldName, State = #state{fd = Fd}) ->
	case reset_entry(next, State) of
		ok -> 
    		case journald_api:get_data(Fd, FieldName) of
    			{ok, Data} -> Data;
    			Error -> Error
    		end;
    	Error ->
    		Error
    end.

reset_cursor(Cursor, #state{fd = Fd}) ->
	journald_api:seek_cursor(Fd, Cursor),
	journald_api:next(Fd),
	case journald_api:test_cursor(Fd, Cursor) of
		ok -> ok;
		_ -> 
			journald_api:seek_cursor(Fd, Cursor),
			journald_api:previous(Fd)
	end,
	journald_api:seek_cursor(Fd, Cursor).

get_last_cursor(#state{fd = Fd}) ->
	ok = journald_api:seek_tail(Fd),
	ok = journald_api:previous(Fd),
	{ok, Cursor} = journald_api:get_cursor(Fd),
	Cursor.

reset_timeframe(DateTime1, DateTime2, State = #state{fd = Fd}) ->
    {ok, Cursor1} = seek_timestamp(DateTime1, State),
    {ok, Cursor2} = seek_timestamp(DateTime2, State),
	case State#state.direction of
		bot -> 
			case Cursor1 of
				undefined ->
					journald_api:seek_head(Fd);
				_ ->
					reset_cursor(Cursor1, State)
			end;
		top -> 
			case Cursor2 of
				undefined ->
					journald_api:seek_tail(Fd);
				_ ->
					reset_cursor(Cursor2, State)
			end
	end,
    TimeFrame = #time_frame{fst_cursor = Cursor1, snd_cursor = Cursor2},
    State#state{time_frame = TimeFrame}.

seek_timestamp(undefined, _State) ->
	{ok, undefined};
seek_timestamp(DateTime, #state{fd = Fd}) ->
    Time = datetime_to_unix_seconds(DateTime),
    ok = journald_api:seek_realtime_usec(Fd, Time),
    case journald_api:next(Fd) of
    	ok -> ok;
    	eaddrnotavail ->
    		journald_api:seek_tail(Fd),
    		journald_api:previous(Fd)
    end,
    {ok, EntryTimeNext} = journald_api:get_realtime_usec(Fd),
    ok = journald_api:seek_realtime_usec(Fd, Time),
    case journald_api:previous(Fd) of
    	ok -> ok;
    	eaddrnotavail ->
    		journald_api:seek_head(Fd),
    		journald_api:next(Fd)
    end,
    {ok, EntryTimePrevious} = journald_api:get_realtime_usec(Fd),
    Diff1 = Time - EntryTimeNext,
    Diff2 = Time - EntryTimePrevious,
    case (abs(Diff1) < abs(Diff2)) of
    	true ->
		    ok = journald_api:seek_realtime_usec(Fd, Time),
		    ok = journald_api:next(Fd),
		    journald_api:get_cursor(Fd);
		false ->
		    journald_api:get_cursor(Fd)
	end.

datetime_to_unix_seconds(DateTime) ->
    DateTimeInSecs = calendar:datetime_to_gregorian_seconds(DateTime),
	UnixEpoch={{1970,1,1},{0,0,0}},
    UnixTimeInSecs = calendar:datetime_to_gregorian_seconds(UnixEpoch),
	1000000*(DateTimeInSecs-UnixTimeInSecs).

move(Pos, #state{fd = Fd, direction = Dir, time_frame = TimeFrame}) ->
	#time_frame{fst_cursor = Cursor1, snd_cursor = Cursor2} = TimeFrame,
	case Pos of
		next ->
			case Dir of 
				bot when Cursor2 /= undefined -> 
					case journald_api:test_cursor(Fd, Cursor2) of
						ok -> eaddrnotavail;
						_ ->
							move1(Fd, next)
					end;
				bot ->
					move1(Fd, next);
				top when Cursor1 /= undefined -> 
					case journald_api:test_cursor(Fd, Cursor1) of
						ok -> eaddrnotavail;
						_ ->
							move1(Fd, previous)
					end;
				top ->
					move1(Fd, previous)
			end;
		head when Cursor1 /= undefined ->
			ok = journald_api:seek_cursor(Fd, Cursor1);
		head ->
			ok = journald_api:seek_head(Fd);
		tail when Cursor2 /= undefined ->
			ok = journald_api:seek_cursor(Fd, Cursor2);
		tail ->
			ok = journald_api:seek_tail(Fd);
		stay -> 
			ok
	end.

move1(Fd, previous) ->							
	Success = journald_api:previous(Fd),
	journald_api:restart_data(Fd),
	Success;
move1(Fd, next) ->							
	Success = journald_api:next(Fd),
	journald_api:restart_data(Fd),
	Success.

register_notifier(Pid, State = #state{fd = Fd, notifier = Notifier}) ->
	#notifier{active = Active, user_pids = Pids} = Notifier,
	case Active of
		false -> ok = journald_api:open_notifier(Fd, self());
		true -> ok
	end,
	NewNotifier = Notifier#notifier{active = true, user_pids = Pids ++ [Pid]},
	State#state{notifier = NewNotifier}.

unregister_notifier(Pid, State = #state{fd = Fd, notifier = Notifier}) ->
	#notifier{active = Active, user_pids = Pids} = Notifier,
	NewPids = lists:delete(Pid, Pids),
	case NewPids of
		[] -> 
			journald_api:close_notifier(Fd),
			Active = false;
		_NotEmpty ->
			Active = true
	end,
	NewNotifier = Notifier#notifier{active = Active, user_pids = NewPids},
	State#state{notifier = NewNotifier}.

evaluate_log_options(Options, State = #state{fd = Fd}) ->
	Dir = proplists:get_value(direction, Options, top),
	AtMost = proplists:get_value(at_most, Options, undefined),
	Since = proplists:get_value(since, Options, undefined),
	Cursor = proplists:get_value(last_cursor, Options, undefined),
	Until = proplists:get_value(until, Options, undefined),
	Field = proplists:get_value(field, Options, undefined),
	case Field of
		undefined 	-> Call = next_entry;
		Field 		-> Call = {next_field, Field}
	end,
	State1 = State#state{direction = Dir},
	State2 = reset_timeframe(Since, Until, State1),
	case Cursor of
		undefined 	->  NewLastCursor = undefined;
		_ 			-> 
			NewLastCursor = get_last_cursor(State),
			reset_cursor(Cursor, State),
			journald_api:next(Fd)
	end,
	Result = collect_logs(Call, AtMost, State2),
	case Cursor of
		undefined 	-> {Result, State2};
		_ 			-> {{Result, NewLastCursor}, State2}
	end.

collect_logs(Call, AtMost, State) ->
	collect_logs(Call, AtMost, State, []).
collect_logs(_Call, 0, _State, Akk) ->
	Akk;
collect_logs(Call, Counter, State, Akk) ->
	case Call of
		next_entry ->
			Result = next_entry(State);
		{next_field, Field} ->
			Result = next_field(Field, State)
	end,
	case Result of
		eaddrnotavail -> Akk;
		Log when Counter =:= undefined ->
			collect_logs(Call, Counter, State, Akk ++ [Log]);
		Log when is_number(Counter) ->
			collect_logs(Call, Counter-1, State, Akk ++ [Log])
	end.

