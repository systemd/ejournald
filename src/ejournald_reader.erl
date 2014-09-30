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
-module(ejournald_reader).
-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1, start_link/2]).

-include("internal.hrl").

-record(time_frame, {
    fst_cursor,
    snd_cursor
}).
-record(state, {
    ctx, 
    time_frame,
    direction
}).

%% ----------------------------------------------------------------------------------------------------
%% -- gen_server callbacks
start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).
    
start_link(Name, Options) ->
    gen_server:start_link({local, Name}, ?MODULE, Options, []).

init(Options) ->
    JournalDir = proplists:get_value(dir, Options),
    case JournalDir of
        undefined   -> 
            {ok, Ctx} = journald_api:open();
        _JournalDir -> 
            BinDir = list_to_binary(JournalDir),
            {ok, Ctx} = journald_api:open_directory(<<BinDir/binary, "\0">>)
    end,
    State = #state{ctx = Ctx, direction = descending, time_frame = #time_frame{}},
    {ok, State}.

handle_call({evaluate, Options}, _From, State) ->
    {Result, NewState} = evaluate_log_options(Options, State),
    {reply, Result, NewState};
handle_call({terminate, Reason}, _From, State) ->
    {stop, Reason, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_info(_Msg, State) -> 
    {noreply, State}.

terminate(_Reason, _State) -> ok.

%% unused
handle_cast(_Msg, State) -> {noreply, State}.
code_change(_,_,State) -> {ok, State}. 

%% ----------------------------------------------------------------------------------------------------
%% -- evaluate options for log retrieval
evaluate_log_options(Options, State = #state{ctx = Ctx}) ->
    Dir = proplists:get_value(direction, Options, descending),
    AtMost = proplists:get_value(at_most, Options),
    Since = proplists:get_value(since, Options),
    Until = proplists:get_value(until, Options),
    Msg = proplists:get_value(message, Options, false),
    State1 = State#state{direction = Dir},
    State2 = reset_timeframe(Since, Until, State1),
    ejournald_helpers:reset_matches(Options, Ctx),
    Result = collect_logs(Msg, AtMost, State2),
    {Result, State2}.

%% ----------------------------------------------------------------------------------------------------
%% -- retrieving logs
next_entry(State = #state{ctx = Ctx}) ->
    case move(next, State) of
        ok -> 
            ejournald_helpers:generate_entry(Ctx);
        Error -> Error
    end.

next_msg(State = #state{ctx = Ctx}) ->
    case move(next, State) of
        ok -> 
            ejournald_helpers:generate_msg(Ctx);
        Error ->
            Error
    end.

%% ------------------------------------------------------------------------
%% -- set pointer
reset_timeframe(DateTime1, DateTime2, State = #state{ctx = Ctx}) ->
    {ok, Cursor1} = ejournald_helpers:seek_timestamp(DateTime1, Ctx),
    {ok, Cursor2} = ejournald_helpers:seek_timestamp(DateTime2, Ctx),
    case State#state.direction of
        ascending -> 
            case Cursor1 of
                undefined ->
                    journald_api:seek_head(Ctx);
                _ ->
                    ejournald_helpers:reset_cursor(Cursor1, Ctx)
            end;
        descending -> 
            case Cursor2 of
                undefined ->
                    journald_api:seek_tail(Ctx);
                _ ->
                    ejournald_helpers:reset_cursor(Cursor2, Ctx)
            end
    end,
    TimeFrame = #time_frame{fst_cursor = Cursor1, snd_cursor = Cursor2},
    State#state{time_frame = TimeFrame}.

%% ------------------------------------------------------------------------------
%% -- pointer movement api
move(next, #state{ctx = Ctx, direction = Dir, time_frame = #time_frame{fst_cursor = Cursor1, snd_cursor = Cursor2}}) ->
    case Dir of 
        ascending when Cursor2 /= undefined -> 
            case journald_api:test_cursor(Ctx, Cursor2) of
                ok -> no_more;
                _ ->
                    ejournald_helpers:move(Ctx, next)
            end;
        ascending ->
            ejournald_helpers:move(Ctx, next);
        descending when Cursor1 /= undefined -> 
            case journald_api:test_cursor(Ctx, Cursor1) of
                ok -> no_more;
                _ ->
                    ejournald_helpers:move(Ctx, previous)
            end;
        descending ->
            ejournald_helpers:move(Ctx, previous)
    end;
move(head, #state{ctx = Ctx, time_frame = #time_frame{fst_cursor = Cursor1}})
    when Cursor1 /= undefined -> ok = journald_api:seek_cursor(Ctx, Cursor1);
move(head, #state{ctx = Ctx}) ->
    ok = journald_api:seek_head(Ctx);
move(tail, #state{ctx = Ctx, time_frame = #time_frame{snd_cursor = Cursor2}})
    when Cursor2 /= undefined -> ok = journald_api:seek_cursor(Ctx, Cursor2);
move(tail, #state{ctx = Ctx}) ->
    ok = journald_api:seek_tail(Ctx).

%% ------------------------------------------------------------------------------
%% -- retrieving logs
collect_logs(Msg, AtMost, State) ->
    collect_logs(Msg, AtMost, State, []).
collect_logs(_Msg, 0, _State, Akk) ->
    lists:reverse(Akk);
collect_logs(Msg, Counter, State, Akk) ->
    case Msg of
        false -> Result = next_entry(State);
        true -> Result = next_msg(State)
    end,
    case Result of
        no_more -> lists:reverse(Akk);
        Log when Counter =:= undefined ->
            collect_logs(Msg, Counter, State, [Log | Akk]);
        Log when is_number(Counter) ->
            collect_logs(Msg, Counter-1, State, [Log | Akk])
    end.

