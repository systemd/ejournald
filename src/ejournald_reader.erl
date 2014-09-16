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
-export([register_notifier/2, unregister_notifier/2]).
-export([start_link/1]).

-define(LOG_LVLS,  [{emergency, 0}, 
                    {alert, 1}, 
                    {critical, 2}, 
                    {error, 3}, 
                    {warning, 4}, 
                    {notice, 5}, 
                    {info, 6}, 
                    {debug, 7}]).

-record(time_frame, {
    fst_cursor,
    snd_cursor
}).
-record(notifier, {
    active,
    user_pids
}).
-record(state, {
    ctx, 
    time_frame,
    direction,
    notifier
}).

%% ----------------------------------------------------------------------------------------------------
%% -- notifier api
register_notifier(Id, Pid) ->
    ok = gen_server:call(Id, {register_notifier, Pid}).


unregister_notifier(Id, Pid) ->
    ok = gen_server:call(Id, {unregister_notifier, Pid}).

%% ----------------------------------------------------------------------------------------------------
%% -- gen_server callbacks
start_link(Options) ->
    gen_server:start_link(?MODULE, [Options], []).

init(_Options) ->
    {ok, Ctx} = journald_api:open(),
    Notifier = #notifier{active = false, user_pids = []},
    State = #state{ctx = Ctx, direction = top, time_frame = #time_frame{}, notifier = Notifier},
    {ok, State}.

handle_call({evaluate, Options}, _From, State) ->
    {Result, NewState} = evaluate_log_options(Options, State),
    {reply, Result, NewState};
handle_call({register_notifier, Pid}, _From, State) ->
    {reply, ok, register_notifier1(Pid, State)};
handle_call({unregister_notifier, Pid}, _From, State) ->
    {reply, ok, unregister_notifier1(Pid, State)};
handle_call(last_entry_cursor, _From, State) ->
    LastEntryMeta = get_last_entry_cursor(State),
    {reply, LastEntryMeta, State};
handle_call({flush_logs, Options}, _From, State) ->
    ResultAndMeta = flush_logs(Options, State),
    {reply, ResultAndMeta, State};
handle_call({terminate, Reason}, _From, State) ->
    {stop, Reason, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_info(JournalInfo, State = #state{notifier = Notifier}) 
    when JournalInfo=:=journal_append; JournalInfo=:=journal_changed -> 
    UserPids = Notifier#notifier.user_pids,
    [ Pid ! JournalInfo || Pid <- UserPids ],
    {noreply, State};
handle_info(_Msg, State) -> 
    {noreply, State}.

terminate(_Reason, State = #state{notifier = Notifier}) -> 
    [ unregister_notifier1(Pid, State) || Pid <- Notifier#notifier.user_pids ].

%% unused
handle_cast(_Msg, State) -> {noreply, State}.
code_change(_,_,State) -> {ok, State}. 

%% ----------------------------------------------------------------------------------------------------
%% -- evaluate options for log retrieval
evaluate_log_options(Options, State) ->
    Dir = proplists:get_value(direction, Options, top),
    AtMost = proplists:get_value(at_most, Options, undefined),
    Since = proplists:get_value(since, Options, undefined),
    Until = proplists:get_value(until, Options, undefined),
    Msg = proplists:get_value(message, Options, undefined),
    case Msg of
        true -> Call = next_message;
        _    -> Call = next_entry
    end,
    State1 = State#state{direction = Dir},
    State2 = reset_timeframe(Since, Until, State1),
    reset_matches(Options, State2),
    Result = collect_logs(Call, AtMost, State2),
    {Result, State2}.

%% ----------------------------------------------------------------------------------------------------
%% -- retrieving logs
next_entry(State = #state{ctx = Ctx}) ->
    case move(next, State) of
        ok -> 
            {Timestamp, Priority} = get_meta_info(Ctx),
            Fields = get_fields(Ctx),
            {Timestamp, Priority, Fields};
        Error -> Error
    end.

get_fields(Ctx) ->
    get_fields(Ctx, []).

get_fields(Ctx, Akk) ->
    case journald_api:enumerate_data(Ctx) of
        {ok, Data} ->
            get_fields(Ctx, [Data | Akk]);
        _ ->
            Akk
    end. 

next_msg(State = #state{ctx = Ctx}) ->
    case move(next, State) of
        ok -> 
            case journald_api:get_data(Ctx, <<"MESSAGE\0">>) of
                {ok, PrefixedData} -> 
                    [_, Data] = binary:split(PrefixedData, [<<"=">>]),
                    {Timestamp, Priority} = get_meta_info(Ctx),
                    {Timestamp, Priority, Data};
                Error -> Error
            end;
        Error ->
            Error
    end.

get_last_entry_cursor(#state{ctx = Ctx}) ->
    ok = journald_api:seek_tail(Ctx),
    ok = journald_api:previous(Ctx),
    {ok, Cursor} = journald_api:get_cursor(Ctx),
    Cursor.

get_meta_info(Ctx) ->
    {ok, UnixTimestamp} = journald_api:get_realtime_usec(Ctx),
    Timestamp = unix_seconds_to_datetime(UnixTimestamp),
    {ok, PriorityBin} = journald_api:get_data(Ctx, <<"PRIORITY\0">>),
    [_, PriorityBinInt] = binary:split(PriorityBin, [<<"=">>]),
    {Priority, _ } = lists:keyfind(binary_to_integer(PriorityBinInt), 2, ?LOG_LVLS),
    {Timestamp, Priority}.

%% ------------------------------------------------------------------------
%% -- set pointer
reset_cursor(Cursor, #state{ctx = Ctx}) ->
    journald_api:seek_cursor(Ctx, Cursor),
    journald_api:next(Ctx),
    case journald_api:test_cursor(Ctx, Cursor) of
        ok -> ok;
        _ -> 
            journald_api:seek_cursor(Ctx, Cursor),
            journald_api:previous(Ctx)
    end,
    journald_api:seek_cursor(Ctx, Cursor).

reset_timeframe(DateTime1, DateTime2, State = #state{ctx = Ctx}) ->
    {ok, Cursor1} = seek_timestamp(DateTime1, State),
    {ok, Cursor2} = seek_timestamp(DateTime2, State),
    case State#state.direction of
        bot -> 
            case Cursor1 of
                undefined ->
                    journald_api:seek_head(Ctx);
                _ ->
                    reset_cursor(Cursor1, State)
            end;
        top -> 
            case Cursor2 of
                undefined ->
                    journald_api:seek_tail(Ctx);
                _ ->
                    reset_cursor(Cursor2, State)
            end
    end,
    TimeFrame = #time_frame{fst_cursor = Cursor1, snd_cursor = Cursor2},
    State#state{time_frame = TimeFrame}.

seek_timestamp(undefined, _State) ->
    {ok, undefined};
seek_timestamp(DateTime, #state{ctx = Ctx}) ->
    Time = datetime_to_unix_seconds(DateTime),
    ok = journald_api:seek_realtime_usec(Ctx, Time),
    case journald_api:next(Ctx) of
        ok -> ok;
        no_more ->
            journald_api:seek_tail(Ctx),
            journald_api:previous(Ctx)
    end,
    {ok, EntryTimeNext} = journald_api:get_realtime_usec(Ctx),
    ok = journald_api:seek_realtime_usec(Ctx, Time),
    case journald_api:previous(Ctx) of
        ok -> ok;
        no_more ->
            journald_api:seek_head(Ctx),
            journald_api:next(Ctx)
    end,
    {ok, EntryTimePrevious} = journald_api:get_realtime_usec(Ctx),
    Diff1 = Time - EntryTimeNext,
    Diff2 = Time - EntryTimePrevious,
    case (abs(Diff1) < abs(Diff2)) of
        true ->
            ok = journald_api:seek_realtime_usec(Ctx, Time),
            ok = journald_api:next(Ctx),
            journald_api:get_cursor(Ctx);
        false ->
            journald_api:get_cursor(Ctx)
    end.

reset_matches(Options, #state{ctx = Ctx}) ->
    LogLvl = proplists:get_value(log_level, Options, info),
    journald_api:flush_matches(Ctx),
    LogLvlInt = proplists:get_value(LogLvl, ?LOG_LVLS),
    BinaryLogLvls = [ integer_to_binary(Lvl) || Lvl <- lists:seq(0, LogLvlInt) ],
    Priority = <<"PRIORITY=">>,
    [ journald_api:add_match(Ctx, <<Priority/binary, Lvl/binary>>) || Lvl <- BinaryLogLvls ],
    ErlNode = proplists:get_value(erl_node, Options, undefined),
    ErlApp = proplists:get_value(erl_app, Options, undefined),
    ErlMod = proplists:get_value(erl_mod, Options, undefined),
    ErlFun = proplists:get_value(erl_fun, Options, undefined),
    add_conjunction(Ctx, <<"ERLANG_NODE=">>, ErlNode),
    add_conjunction(Ctx, <<"SYSLOG_IDENTIFIER=">>, ErlApp),
    add_conjunction(Ctx, <<"CODE_FILE=">>, ErlMod),
    add_conjunction(Ctx, <<"CODE_FUNC=">>, ErlFun).

add_conjunction(_Ctx, _Field, undefined) ->
    ok;
add_conjunction(Ctx, Field, Value) ->
    Value1 = atom_to_binary(Value, latin1),
    ok = journald_api:add_conjunction(Ctx),
    Null = <<"\0">>,
    ok = journald_api:add_match(Ctx, <<Field/binary, Value1/binary, Null/binary>>).

datetime_to_unix_seconds(DateTime) ->
    DateTimeInSecs = calendar:datetime_to_gregorian_seconds(DateTime),
    UnixEpoch={{1970,1,1},{0,0,0}},
    UnixTimeInSecs = calendar:datetime_to_gregorian_seconds(UnixEpoch),
    1000000*(DateTimeInSecs-UnixTimeInSecs).

unix_seconds_to_datetime(UnixTime) ->
    Part1 = UnixTime div 1000000000000,
    Rem1 = UnixTime rem 1000000000000,
    Part2 = Rem1 div 1000000, 
    Part3 = Rem1 rem 1000000, 
    NowTimestamp = {Part1, Part2, Part3},
    calendar:now_to_local_time(NowTimestamp).

%% ------------------------------------------------------------------------------
%% -- pointer movement api
move(next, #state{ctx = Ctx, direction = Dir, time_frame = #time_frame{fst_cursor = Cursor1, snd_cursor = Cursor2}}) ->
    case Dir of 
        bot when Cursor2 /= undefined -> 
            case journald_api:test_cursor(Ctx, Cursor2) of
                ok -> no_more;
                _ ->
                    move1(Ctx, next)
            end;
        bot ->
            move1(Ctx, next);
        top when Cursor1 /= undefined -> 
            case journald_api:test_cursor(Ctx, Cursor1) of
                ok -> no_more;
                _ ->
                    move1(Ctx, previous)
            end;
        top ->
            move1(Ctx, previous)
    end;
move(head, #state{ctx = Ctx, time_frame = #time_frame{fst_cursor = Cursor1}})
    when Cursor1 /= undefined -> ok = journald_api:seek_cursor(Ctx, Cursor1);
move(head, #state{ctx = Ctx}) ->
    ok = journald_api:seek_head(Ctx);
move(tail, #state{ctx = Ctx, time_frame = #time_frame{snd_cursor = Cursor2}})
    when Cursor2 /= undefined -> ok = journald_api:seek_cursor(Ctx, Cursor2);
move(tail, #state{ctx = Ctx}) ->
    ok = journald_api:seek_tail(Ctx).

move1(Ctx, previous) ->                          
    Success = journald_api:previous(Ctx),
    journald_api:restart_data(Ctx),
    Success;
move1(Ctx, next) ->                          
    Success = journald_api:next(Ctx),
    journald_api:restart_data(Ctx),
    Success.

%% ------------------------------------------------------------------------------
%% -- notifier api
register_notifier1(Pid, State = #state{ctx = Ctx, notifier = Notifier}) ->
    #notifier{active = Active, user_pids = Pids} = Notifier,
    case Active of
        false -> ok = journald_api:open_notifier(Ctx, self());
        true -> ok
    end,
    NewNotifier = Notifier#notifier{active = true, user_pids = [Pid | Pids]},
    State#state{notifier = NewNotifier}.

unregister_notifier1(Pid, State = #state{ctx = Ctx, notifier = Notifier}) ->
    #notifier{user_pids = Pids} = Notifier,
    NewPids = lists:delete(Pid, Pids),
    case NewPids of
        [] -> 
            ok = journald_api:close_notifier(Ctx),
            NewActive = false;
        _NotEmpty ->
            NewActive = true
    end,
    NewNotifier = Notifier#notifier{active = NewActive, user_pids = NewPids},
    State#state{notifier = NewNotifier}.

flush_logs(Options, State = #state{ctx = Ctx}) ->
    Cursor = proplists:get_value(last_entry_cursor, Options),
    Msg = proplists:get_value(message, Options, undefined),
    case Msg of
        true -> Call = next_message;
        _    -> Call = next_entry
    end,
    State1 = State#state{direction = bot, time_frame = #time_frame{}},
    reset_cursor(Cursor, State),
    move(next, State1),
    reset_matches(Options, State1),
    Result = collect_logs(Call, undefined, State1),
    case journald_api:get_cursor(Ctx) of
        {ok, NewCursor} -> ok;
        _               -> 
            move1(Ctx, previous),
            {ok, NewCursor} = journald_api:get_cursor(Ctx)
    end,
    {Result, NewCursor}.

%% ------------------------------------------------------------------------------
%% -- collecting logs
collect_logs(Call, AtMost, State) ->
    collect_logs(Call, AtMost, State, []).
collect_logs(_Call, 0, _State, Akk) ->
    lists:reverse(Akk);
collect_logs(Call, Counter, State, Akk) ->
    case Call of
        next_entry ->
            Result = next_entry(State);
        next_message ->
            Result = next_msg(State)
    end,
    case Result of
        no_more -> lists:reverse(Akk);
        Log when Counter =:= undefined ->
            collect_logs(Call, Counter, State, [Log | Akk]);
        Log when is_number(Counter) ->
            collect_logs(Call, Counter-1, State, [Log | Akk])
    end.
