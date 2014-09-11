% Copyright 2010-2014, Travelping GmbH <info@travelping.com>

% Permission is hereby granted, free of charge, to any person obtaining a
% copy of this software and associated documentation files (the "Software"),
% to deal in the Software without restriction, including without limitation
% the rights to use, copy, modify, merge, publish, distribute, sublicense,
% and/or sell copies of the Software, and to permit persons to whom the
% Software is furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
% DEALINGS IN THE SOFTWARE.

%% @private
-module(ejournald_io_server).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    fd_stream,
    mode
}).

%% ----------------------------------------------------------------------------------------------------
%% -- gen_server callbacks
start_link(Options) ->
    gen_server:start_link(?MODULE, [Options], []).

init(Options) ->
    State = evaluate_options(Options),
    {ok, State}.

handle_info({io_request, From, ReplyAs, Request}, State) ->
    case request(Request,State) of
        {Tag, Reply, NewState} when Tag =:= ok; Tag =:= error ->
            reply(From, ReplyAs, Reply),
            {noreply, NewState};
        {stop, Reply, NewState} ->
            reply(From, ReplyAs, Reply),
            {stop, Reply, NewState}
    end;
handle_info(_Unknown, State) ->
    {noreply, State}.

terminate(_Reason, #state{fd_stream = Fd}) -> 
    ok = journald_api:close_fd(Fd),
    ok.

handle_call({terminate, Reason}, _From, State) ->
    {stop, Reason, ok, State};
handle_call(_Msg, _From, State) -> {noreply, State}.

%% unused
handle_cast(_Msg, State) -> {noreply, State}.
code_change(_,_,State) -> {ok, State}. 

%% ----------------------------------------------------------------------------------------------------
%% -- helpers
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
    Fd_stream_name = proplists:get_value(name, Options, <<"ejournald_io_server">>),
    Fd_stream_prio = proplists:get_value(log_level, Options, 5),
    Fd_stream_level_prefix = proplists:get_value(level_prefix, Options, 0),
    Fd_stream = journald_api:stream_fd(Fd_stream_name, Fd_stream_prio, Fd_stream_level_prefix),
    #state{fd_stream = Fd_stream, mode = list}.
