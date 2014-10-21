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
-module(ejournald_helpers).

-include("internal.hrl").

-export([reset_matches/2,
         reset_cursor/2,
         move/2,
         seek_timestamp/2,
         generate_entry/1,
         generate_msg/1,
         check_regex/2]).

reset_matches(Options, Ctx) ->
    LogLvl = proplists:get_value(log_level, Options, info),
    journald_api:flush_matches(Ctx),
    LogLvlInt = proplists:get_value(LogLvl, ?LOG_LVLS),
    BinaryLogLvls = [ integer_to_binary(Lvl) || Lvl <- lists:seq(0, LogLvlInt) ],
    Priority = <<"PRIORITY=">>,
    [ journald_api:add_match(Ctx, <<Priority/binary, Lvl/binary>>) || Lvl <- BinaryLogLvls ],
    add_conjunction(Ctx, <<"ERLANG_NODE=">>, proplists:get_value(erlang_node, Options)),
    add_conjunction(Ctx, <<"SYSLOG_IDENTIFIER=">>, proplists:get_value(application, Options)),
    add_conjunction(Ctx, <<"CODE_FILE=">>, proplists:get_value(code_file, Options)),
    add_conjunction(Ctx, <<"CODE_FUNC=">>, proplists:get_value(function, Options)).

add_conjunction(_Ctx, _Field, undefined) ->
    ok;
add_conjunction(Ctx, Field, Value) ->
    Value1 = atom_to_binary(Value, latin1),
    ok = journald_api:add_conjunction(Ctx),
    ok = journald_api:add_match(Ctx, <<Field/binary, Value1/binary, "\0">>).

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

get_meta_info(Ctx) ->
    {ok, UnixTimestamp} = journald_api:get_realtime_usec(Ctx),
    Timestamp = unix_seconds_to_datetime(UnixTimestamp),
    {ok, PriorityBin} = journald_api:get_data(Ctx, <<"PRIORITY\0">>),
    [_, PriorityBinInt] = binary:split(PriorityBin, [<<"=">>]),
    {Priority, _ } = lists:keyfind(binary_to_integer(PriorityBinInt), 2, ?LOG_LVLS),
    {Timestamp, Priority}.

get_fields(Ctx) ->
    get_fields(Ctx, []).

get_fields(Ctx, Akk) ->
    case journald_api:enumerate_data(Ctx) of
        {ok, Data} ->
            get_fields(Ctx, [Data | Akk]);
        _ ->
            Akk
    end. 

generate_entry(Ctx) -> 
    {Timestamp, Priority} = get_meta_info(Ctx),
    Fields = get_fields(Ctx),
    {Timestamp, Priority, Fields}.

generate_msg(Ctx) -> 
    case journald_api:get_data(Ctx, <<"MESSAGE\0">>) of
        {ok, PrefixedData} -> 
            [_, Data] = binary:split(PrefixedData, [<<"=">>]),
            {Timestamp, Priority} = get_meta_info(Ctx),
            {Timestamp, Priority, Data};
        Error -> Error
    end.

check_regex(undefined, Result) -> Result;
check_regex(Regex, Log = {_,_,Data}) ->
    case check_regex_helper(Regex, Data) of
        true    -> Log;
        false   -> ignore
    end.

check_regex_helper(_Regex, []) -> false;
check_regex_helper(Regex, [ Field | Fields ]) ->
    case re:run(Field, Regex) of
        {match, _}  -> true;
        _           -> check_regex_helper(Regex, Fields)
    end;
check_regex_helper(Regex, Data) ->
    check_regex_helper(Regex, [Data]).

reset_cursor(Cursor, Ctx) ->
    journald_api:seek_cursor(Ctx, Cursor),
    journald_api:next(Ctx),
    case journald_api:test_cursor(Ctx, Cursor) of
        ok -> ok;
        _ -> 
            journald_api:seek_cursor(Ctx, Cursor),
            journald_api:previous(Ctx)
    end,
    journald_api:seek_cursor(Ctx, Cursor).

seek_timestamp(undefined, _Ctx) ->
    {ok, undefined};
seek_timestamp(DateTime, Ctx) ->
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

move(Ctx, previous) ->                          
    Success = journald_api:previous(Ctx),
    journald_api:restart_data(Ctx),
    Success;
move(Ctx, next) ->                          
    Success = journald_api:next(Ctx),
    journald_api:restart_data(Ctx),
    Success.

