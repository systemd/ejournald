-module(ejournald_io_server).

-export([start_link/1, init/1, loop/1, get_line/2, get_chars/3]).

-define(CHARS_PER_REC, 10).

-compile({no_auto_import,[get/1]}).

-record(time_frame, {
	active,
	fst_cursor,
	snd_cursor
	}).

-record(field, {
	read,
	rest,
	name
}).

-record(state, {
	fd, 
	fd_stream,
	field,
	time_frame,
	direction,
	mode,
	eof
}).

start_link(Options) ->
    Pid = spawn_link(?MODULE,init,[Options]),
    {ok, Pid}.

init(Options) ->
	State = evaluate_options(Options),
    loop(State).

loop(State) ->
    receive
		{io_request, From, ReplyAs, Request} ->
		    case request(Request,State) of
				{Tag, Reply, NewState} when Tag =:= ok; Tag =:= error ->
				    reply(From, ReplyAs, Reply),
				    loop(NewState);
				{stop, Reply, _NewState} ->
				    reply(From, ReplyAs, Reply),
				    exit(Reply)
		    end;
		{From, rewind_head} ->
		    From ! {self(), ok},
    		loop(reset_entry(head, State));
		{From, rewind_tail} ->
		    From ! {self(), ok},
    		loop(reset_entry(tail, State));
		{From, set_timeframe, DateTime1, DateTime2} ->
			reply_wrapper(From, set_timeframe(DateTime1, DateTime2, State));
		{From, reset_entry, Pos} ->
			reply_wrapper(From, reset_entry(Pos, State));
		{From, next_entry} ->
			reply_wrapper(From, reset_entry(next, State));
		{From, next_field} ->
			reply_wrapper(From, next_field(State));
		{From, reset_fields} ->
		    From ! {self(), ok},
    		loop(reset_fields(State));
		{From, set_field, FieldName} ->
			reply_wrapper(From, set_field(FieldName, State));
		{From, change_dir, Dir} when Dir =:= bot; Dir =:= top ->
		    From ! {self(), ok},
    		loop(State#state{direction = Dir});
		_Unknown ->
		    loop(State)
    end.

reply(From, ReplyAs, Reply) ->
 	From ! {io_reply, ReplyAs, Reply}.

request({put_chars, Encoding, Chars}, State) ->
    put_chars(unicode:characters_to_list(Chars,Encoding),State);
request({put_chars, Encoding, Module, Function, Args}, State) ->
    try
		request({put_chars, Encoding, apply(Module, Function, Args)}, State)
    catch
		_:_ ->
	    	{error, {error,Function}, State}
    end;

request({get_until, Encoding, _Prompt, M, F, As}, State) ->
    get_until(Encoding, M, F, As, State);
request({get_chars, Encoding, _Prompt, N}, State) ->
    get_until(Encoding, ?MODULE, get_chars, [N], State);
request({get_line, Encoding, _Prompt}, State) ->
    get_until(Encoding, ?MODULE, get_line, [], State);
request({get_geometry,_}, State) ->
    {error, {error,enotsup}, State};
request({setopts, Opts}, State) ->
    setopts(Opts, State);
request(getopts, State) ->
    getopts(State);
request({requests, Reqs}, State) ->
     multi_request(Reqs, {ok, ok, State});
request({put_chars,Chars}, State) ->
    request({put_chars,latin1,Chars}, State);
request({put_chars,M,F,As}, State) ->
    request({put_chars,latin1,M,F,As}, State);
request({get_chars,Prompt,N}, State) ->
    request({get_chars,latin1,Prompt,N}, State);
request({get_line,Prompt}, State) ->
    request({get_line,latin1,Prompt}, State);
request({get_until, Prompt,M,F,As}, State) ->
    request({get_until,latin1,Prompt,M,F,As}, State);
request(_Other, State) ->
    {error, {error, request}, State}.

multi_request([R|Rs], {ok, _Res, State}) ->
    multi_request(Rs, request(R, State));
multi_request([_|_], Error) ->
    Error;
multi_request([], Result) ->
    Result.

put_chars(Chars, #state{fd_stream = Fd_stream} = State) ->
	journald_api:write_fd(Fd_stream, Chars),
    {ok, ok, State}.

get_until(Encoding, Mod, Func, As, #state{mode = M} = State) ->
    case get_loop(Mod, Func, As, State, []) of
		{done, Data, _, NewState} when is_binary(Data); is_list(Data) ->
		    if
				M =:= binary -> 
				    {ok, 
				     unicode:characters_to_binary(Data, unicode, Encoding),
				     NewState
				    };
				true ->
				    case check(Encoding, unicode:characters_to_list(Data, unicode)) of
						{error, _} = E ->
						    {error, E, NewState};
						List ->
						    {ok, List, NewState}
				    end
		    end;
		{done, Data, _, NewState} ->
		    {ok, Data, NewState};
		Error ->
		    {error, Error, State}
    end.

get_loop(M,F,A,State,C) ->
    {NewState,L} = get(State),
    case catch apply(M,F,[C,L|A]) of
		{done, List, FunRest} ->
			#field{read = Read, rest = Rest} = NewState#state.field,
			{Read1, _} = lists:split(length(Read)-length(FunRest), Read),
			Rest1 = FunRest ++ Rest,
			Field1 = #field{read = Read1, rest = Rest1},
		    {done, List, [], NewState#state{field = Field1}};
		{more, NewC} ->
		    get_loop(M,F,A,NewState,NewC);
		_ ->
		    {error,F}
    end.

get(State = #state{field = Field}) ->
	#field{read = Read, rest = Rest} = Field,
	case Rest of
		[] ->
			{State, eof};
		[Char | Rest1] ->
			Field1 = #field{read = Read ++ [Char], rest = Rest1},
			NewState = State#state{field = Field1},
			{NewState, [Char]}
	end.

setopts(Opts0,State) ->
    Opts = proplists:unfold(proplists:substitute_negations([{list,binary}], Opts0)),
    case check_valid_opts(Opts) of
		true ->
		    case proplists:get_value(binary, Opts) of
			  	true ->
					{ok,ok,State#state{mode=binary}};
				false ->
					{ok,ok,State#state{mode=binary}};
			   	_ ->
					{ok,ok,State}
			end;
		false ->
		    {error,{error,enotsup},State}
    end.

check_valid_opts([]) ->
    true;
check_valid_opts([{binary,Bool}|T]) when is_boolean(Bool) ->
    check_valid_opts(T);
check_valid_opts(_) ->
    false.

getopts(#state{mode=M} = S) when M =:= binary ->
    {ok,[{binary, true}],S};
getopts(S) ->
    {ok,[{binary, false}],S}.

check(unicode, List) ->
    List;
check(latin1, List) ->
    try 
		[ throw(not_unicode) || X <- List, X > 255 ],
		List
    catch
		throw:_ ->
	    	{error,{cannot_convert, unicode, latin1}}
    end.

get_line([],eof) ->
    {done,eof,[]};
get_line(ThisFar,eof) ->
    {done,ThisFar,[]};
get_line(ThisFar,[Char]) ->
    case Char of
    	$\n ->
    		{done, ThisFar ++ [$\n], []};
		_NoNewline ->
            {more,ThisFar++[Char]}
    end.

get_chars(ThisFar, eof, _N) ->
	{done, ThisFar, []};
get_chars(ThisFar, [Char], N) when length(ThisFar) + 1 >= N ->
    {done,ThisFar ++ [Char],[]};
get_chars(ThisFar,[Char],_N) ->
	{more,ThisFar ++ [Char]}.

move_head(Pos, #state{fd = Fd, direction = Dir, time_frame = TimeFrame}) ->
	#time_frame{fst_cursor = Cursor1, snd_cursor = Cursor2} = TimeFrame,
	case Pos of
		next ->
			case Dir of 
				bot when Cursor2 /= undefined -> 
					case journald_api:test_cursor(Fd, Cursor2) of
						ok -> ok;
						eaddrnotavail ->
							journald_api:next(Fd)
					end;
				bot ->
					journald_api:next(Fd);
				top when Cursor1 /= undefined -> 
					case journald_api:test_cursor(Fd, Cursor1) of
						ok -> ok;
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

reset_entry(Pos, State) ->
	case move_head(Pos, State) of
		ok ->
			reset_fields(State);
		Error ->
			{Error, State}
	end.

reset_cursor(undefined, State) ->
	reset_fields(State);
reset_cursor(Cursor, State = #state{fd = Fd}) ->
	journald_api:seek_cursor(Fd, Cursor),
	journald_api:next(Fd),
	case journald_api:test_cursor(Fd, Cursor) of
		ok -> ok;
		_ -> 
			journald_api:seek_cursor(Fd, Cursor),
			journald_api:previous(Fd)
	end,
	reset_fields(State).

reset_fields(State = #state{fd = Fd}) ->
	ok = journald_api:restart_data(Fd),
	next_field(State).

next_field(State = #state{fd = Fd}) ->
    case journald_api:enumerate_data(Fd) of
    	{ok, Rest} ->
    		insert_field(Rest, State);
    	_NoMore ->
    		{eaddrnotavail, State}
    end.

set_field(FieldName, State = #state{fd = Fd}) ->
    case journald_api:get_data(Fd, FieldName) of
    	{ok, Msg} ->
    		insert_field(Msg, State);
    	_NoMore ->
    		{eaddrnotavail, State}
    end.

insert_field(Rest, State) ->
	{Name, _} = lists:splitwith(fun(Char) -> Char /= $= end, Rest),
	Field = #field{read = [], rest = Rest, name = Name},
	State#state{field = Field}.

reply_wrapper(From, Result) ->
	case Result of
		{Error, State1} ->
	    	From ! {self(), Error};
	    State1 ->
		    From ! {self(), ok}
	end,
	loop(State1).

evaluate_options(Options) ->
	{ok, Fd} = journald_api:open(),
	Dir = proplists:get_value(direction, Options, bot),
	StartPos = proplists:get_value(position, Options, head),
	Fd_stream_name = proplists:get_value(stream_name, Options, "ejournald_io_server"),
	Fd_stream_prio = proplists:get_value(stream_prio, Options, 5),
	Fd_stream_level_prefix = proplists:get_value(stream_level_prefix, Options, 0),
    Fd_stream = journald_api:stream_fd(Fd_stream_name, Fd_stream_prio, Fd_stream_level_prefix),
    State = #state{fd = Fd, fd_stream = Fd_stream, direction = Dir, mode = list, time_frame = #time_frame{}},
   	reset_entry(StartPos, State).

set_timeframe(DateTime1, DateTime2, State) ->
    {ok, Cursor1} = seek_timestamp(DateTime1, State),
    {ok, Cursor2} = seek_timestamp(DateTime2, State),
	case State#state.direction of
		bot -> State1 = reset_cursor(Cursor1, State);
		top -> State1 = reset_cursor(Cursor2, State)
	end,
    TimeFrame = #time_frame{active = true, fst_cursor = Cursor1, snd_cursor = Cursor2},
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

