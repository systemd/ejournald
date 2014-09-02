-module(ejournald_reader).
-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-record(time_frame, {
	fst_cursor,
	snd_cursor
}).

-record(state, {
	fd, 
	time_frame,
	direction
}).

start_link(Options) ->
	gen_server:start_link(?MODULE, [Options], []).

init(Options) ->
	evaluate_options(Options).

handle_cast(rewind_head, State) ->
	reset_entry(head, State),
    {noreply, State};
handle_cast(rewind_tail, State) ->
	reset_entry(tail, State),
    {noreply, State};
handle_cast({reset_timeframe, DateTime1, DateTime2}, State) ->
	{noreply, reset_timeframe(DateTime1, DateTime2, State)};
handle_cast({change_dir, Dir}, State) when Dir=:=bot; Dir=:=top ->
	{noreply, State#state{direction = Dir}};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_call(next_entry, _From, State) ->
	{Result, NewState} = next_entry(State),
	{reply, Result, NewState};
handle_call({next_field, FieldName}, _From, State) ->
	{Result, NewState} = next_field(FieldName, State),
	{reply, Result, NewState};
handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_,_,State) -> {ok, State}. 

evaluate_options(Options) ->
	{ok, Fd} = journald_api:open(),
	Dir = proplists:get_value(direction, Options, top),
	StartPos = proplists:get_value(position, Options, tail),
    State = #state{fd = Fd, direction = Dir, time_frame = #time_frame{}},
    reset_entry(StartPos, State),
   	{ok, State}.

reset_entry(Pos, State = #state{fd = Fd}) ->
	case move(Pos, State) of
		ok -> journald_api:restart_data(Fd);
		Error -> Error
	end.

next_entry(State = #state{fd = Fd}) ->
	case reset_entry(next, State) of
		ok -> {get_fields(Fd), State};
		Error -> {Error, State}
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
    			{ok, Data} -> {Data, State};
    			Error -> {Error, State}
    		end;
    	Error ->
    		{Error, State}
    end.

reset_cursor(Cursor, State = #state{fd = Fd}) ->
	journald_api:seek_cursor(Fd, Cursor),
	journald_api:next(Fd),
	case journald_api:test_cursor(Fd, Cursor) of
		ok -> ok;
		_ -> 
			journald_api:seek_cursor(Fd, Cursor),
			journald_api:previous(Fd)
	end,
	State.

reset_timeframe(DateTime1, DateTime2, State = #state{fd = Fd}) ->
    {ok, Cursor1} = seek_timestamp(DateTime1, State),
    {ok, Cursor2} = seek_timestamp(DateTime2, State),
	case State#state.direction of
		bot -> 
			case Cursor1 of
				undefined ->
					journald_api:seek_head(Fd),
					journald_api:next(Fd),
					State1 = State;
				_ ->
					State1 = reset_cursor(Cursor1, State)
			end;
		top -> 
			case Cursor2 of
				undefined ->
					journald_api:seek_tail(Fd),
					journald_api:previous(Fd),
					State1 = State;
				_ ->
					State1 = reset_cursor(Cursor2, State)
			end
	end,
    TimeFrame = #time_frame{fst_cursor = Cursor1, snd_cursor = Cursor2},
    State1#state{time_frame = TimeFrame}.

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
						eaddrnotavail ->
							journald_api:next(Fd)
					end;
				bot ->
					journald_api:next(Fd);
				top when Cursor1 /= undefined -> 
					case journald_api:test_cursor(Fd, Cursor1) of
						ok -> eaddrnotavail;
						eaddrnotavail ->
							journald_api:previous(Fd)
					end;
				top ->
					journald_api:previous(Fd)
			end;
		head when Cursor1 /= undefined ->
			ok = journald_api:seek_cursor(Fd, Cursor1),
			journald_api:previous(Fd);
		head ->
			ok = journald_api:seek_head(Fd),
			journald_api:next(Fd);
		tail when Cursor2 /= undefined ->
			ok = journald_api:seek_cursor(Fd, Cursor2),
			journald_api:next(Fd);
		tail ->
			ok = journald_api:seek_tail(Fd),
			journald_api:previous(Fd);
		stay -> 
			ok
	end.

