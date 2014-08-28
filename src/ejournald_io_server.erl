-module(ejournald_io_server).

-export([start_link/1, init/1, loop/1, get_line/2, get_chars/3]).

-define(CHARS_PER_REC, 10).

-compile({no_auto_import,[get/1]}).

-record(field, {read,
				rest
}).

-record(position, {	cursor,
					byte_pos,
					eof
}).

-record(state, {
	  fd, 
	  fd_stream,
	  field,
	  position,	
	  direction,
	  mode 
}).

start_link(Options) ->
    spawn_link(?MODULE,init,[Options]).

init(Options) ->
	{ok, Fd} = journald_api:open(),
	Dir = proplists:get_value(direction, Options, bot),
	case Dir of
		bot ->
			ok = journald_api:seek_head(Fd),
			ok = journald_api:next(Fd);
		top ->
			ok = journald_api:seek_tail(Fd),
			ok = journald_api:previous(Fd)
	end,
    Fd_stream = journald_api:stream_fd("ejournald_io_server", 5, 0),
    Cursor = journald_api:get_cursor(Fd),
    {ok, Rest} = journald_api:enumerate_data(Fd),
    Field = #field{read = [], rest = Rest ++ [$\n]},
    Position = #position{cursor = Cursor, byte_pos = 0, eof = false},
    loop(#state{fd = Fd, fd_stream = Fd_stream, field = Field, position = Position, mode=list, direction = Dir}).

loop(State = #state{fd = Fd, position = Pos}) ->
    receive
		{io_request, From, ReplyAs, Request} ->
		    case request(Request,State) of
				{Tag, Reply, NewState} when Tag =:= ok; Tag =:= error ->
				    reply(From, ReplyAs, Reply),
				    ?MODULE:loop(NewState);
				{stop, Reply, _NewState} ->
				    reply(From, ReplyAs, Reply),
				    exit(Reply)
		    end;
		{From, rewind_top} ->
			journald_api:seek_head(Fd),
			journald_api:next(Fd),
		    NewCursor = journald_api:get_cursor(Fd),
		   	ok = journald_api:restart_data(Fd),
		    {ok, Rest} = journald_api:enumerate_data(Fd),
		    Field = #field{read = [], rest = Rest ++ [$\n]},
		    NewPos = #position{cursor = NewCursor, byte_pos = 0, eof = false},
		    From ! {self(), ok},
    		loop(State#state{field = Field, position = NewPos, direction = bot});
		{From, rewind_bot} ->
			journald_api:seek_tail(Fd),
			journald_api:previous(Fd),
		    NewCursor = journald_api:get_cursor(Fd),
		   	ok = journald_api:restart_data(Fd),
		    {ok, Rest} = journald_api:enumerate_data(Fd),
		    Field = #field{read = [], rest = Rest ++ [$\n]},
		    NewPos = #position{cursor = NewCursor, byte_pos = 0, eof = false},
		    From ! {self(), ok},
    		loop(State#state{field = Field, position = NewPos, direction = top});
		{From, change_dir, Dir} when Dir =:= bot; Dir =:= top ->
		   	ok = journald_api:restart_data(Fd),
		    {ok, Rest} = journald_api:enumerate_data(Fd),
		    Field = #field{read = [], rest = Rest ++ [$\n]},
		    NewPos = Pos#position{byte_pos = 0, eof = false},
		    From ! {self(), ok},
    		loop(State#state{field = Field, position = NewPos, direction = Dir});
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
		{done, List, _Rest} ->
		    {done, List, [], NewState};
		{more, NewC} ->
		    get_loop(M,F,A,NewState,NewC);
		_ ->
		    {error,F}
    end.

get(State = #state{position = #position{eof = true}}) ->
	{State, eof};
get(State = #state{fd = Fd, field = Field, position = Pos, direction = Dir}) ->
	#field{read = Read, rest = Rest} = Field,
	#position{byte_pos = BytePos} = Pos,
	case Rest of
		[] ->
			case journald_api:enumerate_data(Fd) of
				{ok, [Char | Rest1]} ->
					Field1 = #field{read = [Char], rest = Rest1 ++ [$\n]},
					Pos1 = Pos#position{byte_pos = 1},
					NewState = State#state{field = Field1, position = Pos1},
					{NewState, [Char]};
				eaddrnotavail ->
					case Dir of
						bot ->
							DirTest = journald_api:next(Fd);
						top ->
							DirTest = journald_api:previous(Fd)
					end,
					case DirTest of
						eaddrnotavail ->
							Field1 = #field{read = undefined, rest = undefined},
							Pos1 = Pos#position{cursor = undefined, byte_pos = 0, eof = true},
							NewState = State#state{field = Field1, position = Pos1},
							{NewState, eof};
						ok ->
							NewCursor = journald_api:get_cursor(Fd),
							{ok, [Char | Rest1]} = journald_api:enumerate_data(Fd),
							Field1 = #field{read = [Char], rest = Rest1 ++ [$\n]},
							Pos1 = Pos#position{cursor = NewCursor, byte_pos = 1},
							NewState = State#state{field = Field1, position = Pos1},
							{NewState, [Char]}
					end
			end;
		[Char | Rest1] ->
			Field1 = #field{read = Read ++ [Char], rest = Rest1},
			Pos1 = Pos#position{byte_pos = BytePos + 1},
			NewState = State#state{field = Field1, position = Pos1},
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

get_chars(ThisFar, [Char], N) when length(ThisFar) + 1 >= N ->
    {done,ThisFar ++ [Char],[]};
get_chars(ThisFar,[Char],_N) ->
	{more,ThisFar ++ [Char]}.

