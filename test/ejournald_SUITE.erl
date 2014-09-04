-module(ejournald_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(READER, ejournald_reader).
-define(IO_SERVER, ejournald_io_server).

-define(READ, {read, [], [read_last_3_logs, read_last_3_messages, read_since, read_since_until, notify]}).
-define(WRITE, {write, [], [io_put_chars, io_format, io_write, io_fwrite]}).

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
	ok = io:fwrite(?IO_SERVER, "Some weird string by fwrite:", []),
	ok = io:fwrite(?IO_SERVER, "|~10.5c|~-10.5c|~5c|~n", [$a, $b, $c]),
	ok = io:nl(?IO_SERVER).

%% ----------------------------------------------------------------------------------------------------
%% -- testcases READING
read_last_3_logs(_Config) ->
	Logs = ejournald:get_logs([{direction, top}, {at_most, 3}]),
	[ ct:log(Log) || Log <- Logs ],
	ok.

read_last_3_messages(_Config) ->
	Logs = ejournald:get_logs([{direction, top}, {at_most, 3}, {field, "MESSAGE"}]),
	[ ct:log(Log) || Log <- Logs ],
	ok.

read_since(_Config) ->
	DateTime = calendar:now_to_universal_time(erlang:now()),
	{Date, {H, M, S}} = DateTime,
	Since = {Date, {H, M-5, S}},
	Logs = ejournald:get_logs([{direction, bot}, {since, Since}, {field, "MESSAGE"}]),
	[ ct:log(Log) || Log <- Logs ],
	ok.

read_since_until(_Config) -> %% don't try this test around midnight ;) (the days could swap in a bad way)
	DateTime = calendar:now_to_universal_time(erlang:now()),
	{Date, {H, M, S}} = DateTime,
	Since = {Date, {mod(H-1, 24), mod(M-10, 60), S}},
	Until = {Date, {mod(H-1, 24), mod(M-5, 60), S}},
	Logs = ejournald:get_logs([{direction, top}, {at_most, 10}, {since, Since}, {until, Until}, {field, "MESSAGE"}]),
	[ ct:log(Log) || Log <- Logs ],
	ok.

notify(_Config) ->
	{ok, _Pid} = ejournald:log_notify(self(), [{field, "MESSAGE"}]),
	ok = io:format(?IO_SERVER, "1~n", []),
	ok = io:format(?IO_SERVER, "2~n", []),
	ok = io:format(?IO_SERVER, "3~n", []),
	receive_flush(3).

%% ----------------------------------------------------------------------------------------------------
%% -- ct callbacks
groups() -> [?READ, ?WRITE].

all() -> [{group, write}, {group, read}].

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
		Log -> 
			ct:log(Log),
			receive_flush(N-1)
	after
		200 ->
			erlang:error(timeout)
	end.