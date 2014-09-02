-module(ejournald).
-behaviour(application).

-export([start/2, stop/1]).
-export([start_io/1, start_io/2, stop_io/1,
		 start_reader/1, start_reader/2, stop_reader/1,
		 get_last/1, get_timeframe/2
		]).

-define(READER, ejournald_reader).
-define(IO_SERVER, ejournald_io_server).

%% ----------------------------------------------------------------------------------------------------
%% -- application callbacks
start(_Type, _Args) ->
	ejournald_sup:start_link(),
	start_reader(?READER, []).
	%start_io(?IO_SERVER).

stop(_State) ->
	ok.

%% ----------------------------------------------------------------------------------------------------
%% -- interface for ejournald_io_server
start_io(Options) ->
	ejournald_sup:start(?IO_SERVER, Options).

start_io(Name, Options) ->
	ejournald_sup:start(?IO_SERVER, Name, Options).

stop_io(Id) ->
	ejournald_sup:stop(Id).

%% ----------------------------------------------------------------------------------------------------
%% -- API for ejournald_reader
start_reader(Options) ->
	ejournald_sup:start(?READER, Options).

start_reader(Name, Options) ->
	ejournald_sup:start(?READER, Name, Options).

stop_reader(Id) ->
	ejournald_sup:stop(Id).

%% ----------------------------------------------------------------------------------------------------
%% -- API for retrieving logs
get_last(N) ->
	gen_server:cast(?READER, rewind_tail),
	[ gen_server:call(?READER, {next_field, "MESSAGE"}) || _ <- lists:seq(1,N) ].

get_timeframe(Start, End) ->
	gen_server:cast(?READER, {reset_timeframe, Start, End}),
	collect_logs().

%% ----------------------------------------------------------------------------------------------------
%% -- helpers
collect_logs() ->
	collect_logs([]).
collect_logs(Akk) ->
	Log = gen_server:call(?READER, next_entry),
	case Log of
		eaddrnotavail ->
			Akk;
		Log ->
		erlang:display(Log),
			collect_logs(Akk ++ [Log])
	end.



