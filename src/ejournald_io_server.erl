-module(ejournald_io_server).

-export([start_link/1, init/1, loop/1, get_line/2, get_chars/3]).

-record(state, {
	fd_stream,
	mode
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
    case catch apply(M,F,[C,eof|A]) of
		{done, List, _FunRest} ->
		    {done, List, [], State};
		{more, NewC} ->
		    get_loop(M,F,A,State,NewC);
		_ ->
		    {error,F}
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

evaluate_options(Options) ->
	Fd_stream_name = proplists:get_value(stream_name, Options, "ejournald_io_server"),
	Fd_stream_prio = proplists:get_value(stream_prio, Options, 5),
	Fd_stream_level_prefix = proplists:get_value(stream_level_prefix, Options, 0),
    Fd_stream = journald_api:stream_fd(Fd_stream_name, Fd_stream_prio, Fd_stream_level_prefix),
    #state{fd_stream = Fd_stream, mode = list}.
