-module(ejournald).
-behaviour(application).

-export([start/2, stop/1]).
-export([start_io/1, start_io/2, stop_io/1,
		 start_reader/1, start_reader/2, stop_reader/1,
		 get_logs/1, get_logs/2
		]).
-export([log_notify/2, log_notify/3,
		 log_notify_worker/3
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
	get_logs(?READER, Options).

get_logs(Id, Options) ->
	gen_server:call(Id, {evaluate, Options}).

log_notify(Sink, Options) ->
	log_notify(?READER, Sink, Options).

log_notify(Id, Sink, Options) ->
	evaluate_options_notify(Id, Sink, Options).

%% ----------------------------------------------------------------------------------------------------
%% -- helpers
evaluate_options_notify(Id, Sink, Options) -> 
	case Sink of
		undefined -> erlang:error(badarg, {error, no_sink});
		_Sink -> ok
	end,
	Cursor = gen_server:call(Id, last_entry_cursor),
	Pid = spawn(?MODULE, log_notify_worker, [Id, Sink, [ {last_entry_cursor, Cursor} | Options ]]),
	ok = gen_server:call(Id, {register_notifier, Pid}),
	{ok, Pid}.

log_notify_worker(Id, Sink, Options) ->
	receive 
		journal_append ->
			{Result, Cursor} = gen_server:call(Id, {flush_logs, Options}),
			evaluate_sink(Sink, Result),
			NewOptions = lists:keyreplace(last_entry_cursor, 1, Options, {last_entry_cursor, Cursor}),
			log_notify_worker(Id, Sink, NewOptions);
		{'DOWN', _Ref, process, Sink, _Reason} ->
			gen_server:call(Id, {unregister_notifier, self()});
		exit ->
			gen_server:call(Id, {unregister_notifier, self()})
	end.

evaluate_sink(_Sink, []) -> ok;
evaluate_sink(Sink, [{Timestamp, Log} | Result]) when is_pid(Sink) ->
	Sink ! {Timestamp, Log},
	evaluate_sink(Sink, Result);
evaluate_sink(Sink, [ TsLog = {_Timestamp, _Log} | Result]) when is_function(Sink,1) ->
	catch(Sink(TsLog)),
	evaluate_sink(Sink, Result);
evaluate_sink(Sink, [ _ | Result]) when is_pid(Sink);is_function(Sink,1) ->
	evaluate_sink(Sink, Result);
evaluate_sink(_Sink, _Result) ->
	erlang:error(badarg).

