-module(ejournald_io_server).

-export([start_link/1, init/1, loop/1]).

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
request(_Other, State) ->
    {error, eof, State}.

multi_request([R|Rs], {ok, _Res, State}) ->
    multi_request(Rs, request(R, State));
multi_request([_|_], Error) ->
    Error;
multi_request([], Result) ->
    Result.

put_chars(Chars, #state{fd_stream = Fd_stream} = State) ->
	journald_api:write_fd(Fd_stream, Chars),
    {ok, ok, State}.

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

evaluate_options(Options) ->
	Fd_stream_name = proplists:get_value(stream_name, Options, "ejournald_io_server"),
	Fd_stream_prio = proplists:get_value(stream_prio, Options, 5),
	Fd_stream_level_prefix = proplists:get_value(stream_level_prefix, Options, 0),
    Fd_stream = journald_api:stream_fd(Fd_stream_name, Fd_stream_prio, Fd_stream_level_prefix),
    #state{fd_stream = Fd_stream, mode = list}.
