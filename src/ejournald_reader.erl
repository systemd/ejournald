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
    fd, 
    time_frame,
    direction,
    notifier
}).

%% ----------------------------------------------------------------------------------------------------
%% -- gen_server callbacks
start_link(Options) ->
    gen_server:start_link(?MODULE, [Options], []).

init(_Options) ->
    {ok, Fd} = journald_api:open(),
    Notifier = #notifier{active = false, user_pids = []},
    State = #state{fd = Fd, direction = top, time_frame = #time_frame{}, notifier = Notifier},
    {ok, State}.

handle_call({evaluate, Options}, _From, State) ->
    {Result, NewState} = evaluate_log_options(Options, State),
    {reply, Result, NewState};
handle_call({register_notifier, Pid}, _From, State) ->
    {reply, ok, register_notifier(Pid, State)};
handle_call({unregister_notifier, Pid}, _From, State) ->
    {reply, ok, unregister_notifier(Pid, State)};
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
    [ unregister_notifier(Pid, State) || Pid <- Notifier#notifier.user_pids ].

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
next_entry(State = #state{fd = Fd}) ->
    case move(next, State) of
        ok -> 
            {Timestamp, Priority} = get_meta_info(Fd),
            Fields = get_fields(Fd),
            {Timestamp, Priority, Fields};
        Error -> Error
    end.

get_fields(Fd) ->
    get_fields(Fd, []).

get_fields(Fd, Akk) ->
    case journald_api:enumerate_data(Fd) of
        {ok, Data} ->
            get_fields(Fd, [Data | Akk]);
        _ ->
            Akk
    end. 

next_msg(State = #state{fd = Fd}) ->
    case move(next, State) of
        ok -> 
            case journald_api:get_data(Fd, <<"MESSAGE\0">>) of
                {ok, PrefixedData} -> 
                    [_, Data] = binary:split(PrefixedData, [<<"=">>]),
                    {Timestamp, Priority} = get_meta_info(Fd),
                    {Timestamp, Priority, Data};
                Error -> Error
            end;
        Error ->
            Error
    end.

get_last_entry_cursor(#state{fd = Fd}) ->
    ok = journald_api:seek_tail(Fd),
    ok = journald_api:previous(Fd),
    {ok, Cursor} = journald_api:get_cursor(Fd),
    Cursor.

get_meta_info(Fd) ->
    {ok, UnixTimestamp} = journald_api:get_realtime_usec(Fd),
    Timestamp = unix_seconds_to_datetime(UnixTimestamp),
    {ok, PriorityBin} = journald_api:get_data(Fd, <<"PRIORITY\0">>),
    [_, PriorityBinInt] = binary:split(PriorityBin, [<<"=">>]),
    {Priority, _ } = lists:keyfind(binary_to_integer(PriorityBinInt), 2, ?LOG_LVLS),
    {Timestamp, Priority}.

%% ------------------------------------------------------------------------
%% -- set pointer
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
        no_more ->
            journald_api:seek_tail(Fd),
            journald_api:previous(Fd)
    end,
    {ok, EntryTimeNext} = journald_api:get_realtime_usec(Fd),
    ok = journald_api:seek_realtime_usec(Fd, Time),
    case journald_api:previous(Fd) of
        ok -> ok;
        no_more ->
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

reset_matches(Options, #state{fd = Fd}) ->
    LogLvl = proplists:get_value(log_level, Options, info),
    journald_api:flush_matches(Fd),
    LogLvlInt = proplists:get_value(LogLvl, ?LOG_LVLS),
    BinaryLogLvls = [ integer_to_binary(Lvl) || Lvl <- lists:seq(0, LogLvlInt) ],
    Priority = <<"PRIORITY=">>,
    [ journald_api:add_match(Fd, <<Priority/binary, Lvl/binary>>) || Lvl <- BinaryLogLvls ],
    ErlNode = proplists:get_value(erl_node, Options, undefined),
    ErlApp = proplists:get_value(erl_app, Options, undefined),
    ErlMod = proplists:get_value(erl_mod, Options, undefined),
    ErlFun = proplists:get_value(erl_fun, Options, undefined),
    add_conjunction(Fd, <<"ERLANG_NODE=">>, ErlNode),
    add_conjunction(Fd, <<"SYSLOG_IDENTIFIER=">>, ErlApp),
    add_conjunction(Fd, <<"CODE_FILE=">>, ErlMod),
    add_conjunction(Fd, <<"CODE_FUNC=">>, ErlFun).

add_conjunction(_Fd, _Field, undefined) ->
    ok;
add_conjunction(Fd, Field, Value) ->
    Value1 = atom_to_binary(Value, latin1),
    ok = journald_api:add_conjunction(Fd),
    Null = <<"\0">>,
    ok = journald_api:add_match(Fd, <<Field/binary, Value1/binary, Null/binary>>).

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
move(Pos, #state{fd = Fd, direction = Dir, time_frame = TimeFrame}) ->
    #time_frame{fst_cursor = Cursor1, snd_cursor = Cursor2} = TimeFrame,
    case Pos of
        next ->
            case Dir of 
                bot when Cursor2 /= undefined -> 
                    case journald_api:test_cursor(Fd, Cursor2) of
                        ok -> no_more;
                        _ ->
                            move1(Fd, next)
                    end;
                bot ->
                    move1(Fd, next);
                top when Cursor1 /= undefined -> 
                    case journald_api:test_cursor(Fd, Cursor1) of
                        ok -> no_more;
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

%% ------------------------------------------------------------------------------
%% -- notifier api
register_notifier(Pid, State = #state{fd = Fd, notifier = Notifier}) ->
    #notifier{active = Active, user_pids = Pids} = Notifier,
    case Active of
        false -> ok = journald_api:open_notifier(Fd, self());
        true -> ok
    end,
    NewNotifier = Notifier#notifier{active = true, user_pids = [Pid | Pids]},
    State#state{notifier = NewNotifier}.

unregister_notifier(Pid, State = #state{fd = Fd, notifier = Notifier}) ->
    #notifier{user_pids = Pids} = Notifier,
    NewPids = lists:delete(Pid, Pids),
    case NewPids of
        [] -> 
            ok = journald_api:close_notifier(Fd),
            NewActive = false;
        _NotEmpty ->
            NewActive = true
    end,
    NewNotifier = Notifier#notifier{active = NewActive, user_pids = NewPids},
    State#state{notifier = NewNotifier}.

flush_logs(Options, State = #state{fd = Fd}) ->
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
    case journald_api:get_cursor(Fd) of
        {ok, NewCursor} -> ok;
        _               -> 
            move1(Fd, previous),
            {ok, NewCursor} = journald_api:get_cursor(Fd)
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
