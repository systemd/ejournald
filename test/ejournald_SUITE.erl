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

-module(ejournald_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(READER, ejournald_reader).
-define(IO_SERVER, journald).

-define(CONTROL, {control, [], [start_stop_io, start_stop_reader]}).
-define(READ, {read, [], [read_last_3_logs, read_last_3_messages, read_since, read_since_until, notify]}).
-define(WRITE, {write, [], [io_put_chars, io_format, io_write, io_fwrite]}).

%% ----------------------------------------------------------------------------------------------------
%% -- testcases CONTROL
start_stop_io(_Config) ->
    {ok, _Pid} = ejournald:start_io(test_io, [{name, <<"test_io">>}]),
    ok = ejournald:stop_io(test_io).

start_stop_reader(_Config) ->
    {ok, _Pid} = ejournald:start_reader(test_reader),
    ok = ejournald:stop_reader(test_reader).

%% ----------------------------------------------------------------------------------------------------
%% -- testcases WRITING
io_put_chars(_Config) -> 
    ok = io:put_chars(?IO_SERVER, "put_chars1"),
    ok = io:put_chars(?IO_SERVER, " put_chars2"),
    ok = io:put_chars(?IO_SERVER, " written by io:put_char"),
    ok = io:nl(?IO_SERVER).

io_format(_Config) ->
    ok = io:format(?IO_SERVER, "written by format~n", []).

io_write(_Config) ->
    ok = io:write(?IO_SERVER, ["I", "am", "a", "list", "written"]),
    ok = io:write(?IO_SERVER, " by io:write"),
    ok = io:nl(?IO_SERVER).

io_fwrite(_Config) ->
    ok = io:fwrite(?IO_SERVER, "Some string by fwrite:", []),
    ok = io:fwrite(?IO_SERVER, "|~10.5c|~-10.5c|~5c|~n", [$a, $b, $c]),
    ok = io:nl(?IO_SERVER).

%% ----------------------------------------------------------------------------------------------------
%% -- testcases READING
read_last_3_logs(_Config) ->
    Logs = ejournald:get_logs([{direction, descending}, {at_most, 3}]),
    [ [ ct:log(binary_to_list(Field)) || Field <- Log ++ [<<"~n">>] ] || {_Timestamp, _Priority, Log} <- Logs ],
    ok.

read_last_3_messages(_Config) ->
    Logs = ejournald:get_logs([{direction, descending}, {at_most, 3}, {message, true}]),
    [ ct:log(binary_to_list(Log)) || {_Timestamp, _Priority, Log} <- Logs ],
    ok.

read_since(_Config) ->
    DateTime = calendar:now_to_universal_time(erlang:now()),
    {Date, {H, M, S}} = DateTime,
    Since = {Date, {H, M-5, S}},
    Logs = ejournald:get_logs([{direction, ascending}, {since, Since}, {message, true}]),
    [ ct:log(binary_to_list(Log)) || {_Timestamp, _Priority, Log} <- Logs ],
    ok.

read_since_until(_Config) -> %% don't try this test around midnight ;) (the days could swap in a bad way)
    DateTime = calendar:now_to_universal_time(erlang:now()),
    {Date, {H, M, S}} = DateTime,
    Since = {Date, {mod(H-1, 24), mod(M-10, 60), S}},
    Until = {Date, {mod(H-1, 24), mod(M-5, 60), S}},
    Logs = ejournald:get_logs([{direction, descending}, {at_most, 10}, {since, Since}, {until, Until}, {message, true}]),
    [ ct:log(binary_to_list(Log)) || {_Timestamp, _Priority, Log} <- Logs ],
    ok.

notify(_Config) ->
    {ok, _Pid} = ejournald:log_notify(self(), [{message, true}]),
    ok = io:format(?IO_SERVER, "1~n", []),
    ok = io:format(?IO_SERVER, "2~n", []),
    ok = io:format(?IO_SERVER, "3~n", []),
    receive_flush(3).

%% ----------------------------------------------------------------------------------------------------
%% -- ct callbacks
groups() -> [?CONTROL, ?READ, ?WRITE].

all() -> [{group, control}, {group, write}, {group, read}].

init_per_suite(Config) ->
    application:start(ejournald),
    Config.
    
end_per_suite(_Config) ->
    application:stop(ejournald).

%% ----------------------------------------------------------------------------------------------------
%% -- helpers
mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,_Y) -> 0.

receive_flush(0) -> ok;
receive_flush(N) ->
    receive
        {_Timestamp, _Priority, Log} -> 
            ct:log(binary_to_list(Log)),
            receive_flush(N-1);
        journal_invalidate ->
            ok
    after
        500 ->
            erlang:error(timeout)
    end.