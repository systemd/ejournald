-module(ejournald).
-behaviour(application).

-export([start/2, stop/1]).
-export([start_io/1, start_io/2, stop_io/1,
		 start_reader/1, start_reader/2, stop_reader/1,
		 get_logs/1
		]).
-export([log_notify/2,
		 log_notify_worker/2
		]).

-define(READER, ejournald_reader).
-define(IO_SERVER, ejournald_io_server).

%% ----------------------------------------------------------------------------------------------------
%% -- application callbacks
start(_Type, _Args) ->
	ejournald_sup:start_link(),
	start_reader(?READER, []),
	start_io(?IO_SERVER, []).

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
get_logs(Options) ->
	gen_server:call(?READER, {evaluate, Options}).

log_notify(Sink, Options) ->
	evaluate_options_notify(Sink, Options).

%% ----------------------------------------------------------------------------------------------------
%% -- helpers
evaluate_options_notify(Sink, Options) -> 
	case Sink of
		undefined -> erlang:error(badarg, {error, no_sink});
		_Sink -> ok
	end,
	Cursor = gen_server:call(?READER, last_cursor),
	Pid = spawn(?MODULE, log_notify_worker, [Sink, Options ++ [{last_cursor, Cursor}] ]),
	ok = gen_server:call(?READER, {register_notifier, Pid}).

log_notify_worker(Sink, Options) ->
	receive 
		journal_append ->
			Options1 = [{direction, bot}] ++ Options,
			{Result, Cursor} = get_logs(Options1),
			evaluate_sink(Sink, Result),
			log_notify_worker(Sink, proplists:delete(last_cursor, Options) ++ [{last_cursor, Cursor}]);
		{'DOWN', _Ref, process, Sink, _Reason} ->
			gen_server:call(?READER, {unregister_notifier, self()})
	end.

evaluate_sink(Sink, Result) when is_pid(Sink) ->
	[ Sink ! Log || Log <- Result ];
evaluate_sink(Sink, Result) when is_function(Sink) ->
	[ Sink(Log) || Log <- Result ].




