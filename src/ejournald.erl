-module(ejournald).
-behaviour(application).

-export([start/2, stop/1]).
-export([start_io/1, start_io/2, stop_io/1,
		 start_reader/0, start_reader/1, stop_reader/1,
		 get_logs/1, get_logs/2
		]).
-export([log_notify/2, log_notify/3,
		 log_notify_worker/3
		]).

-define(READER, ejournald_reader).
-define(IO_SERVER, ejournald_io_server).

%% ----------------------------------------------------------------------------------------------------
%% -- types
-type log_level()		::	emergency | 
							alert |
							critical |
							error | 
							warning | 
							notice | 
							info |
 							debug.
 							
-type io_options() 		::	{name, string()} |
							{log_level, log_level()} |
							{level_prefix, string()}.

-type direction()		::	bot | top.
-type datetime1970()	::	calendar:datetime1970().
-type log_options()		:: 	{direction, direction()} |
							{since,	datetime1970()} |
							{until,	datetime1970()} |
							{at_most, integer()} |
							{log_level, log_level()} |
							{message, boolean()}.
-type notify_options()	:: 	{message, boolean()}.
-type sink_fun()		::	fun( (log_message()) -> any() ).
-type sink()			:: 	pid() | sink_fun().
-type log_data()		:: 	string() | [ string() ]. %% depends on the 'message' option
-type log_message()		:: 	{datetime1970(), log_level(), log_data()}.
-type id()				:: 	term() | pid().

%% ----------------------------------------------------------------------------------------------------
%% -- application callbacks
start(_Type, _Args) ->
	ejournald_sup:start_link(),
	start_reader(?READER),
	start_io(?IO_SERVER, []).

stop(_State) ->
	ok.

%% ----------------------------------------------------------------------------------------------------
%% -- interface for ejournald_io_server
-spec start_io( [io_options()] ) -> {ok, pid()} | {error, any()}.
start_io(Options) ->
	ejournald_sup:start(?IO_SERVER, Options).

-spec start_io( term(), [io_options()] ) -> {ok, pid()} | {error, any()}.
start_io(Name, Options) ->
	ejournald_sup:start(?IO_SERVER, Name, Options).

-spec stop_io( term() ) -> ok | {error, any()}.
stop_io(Id) ->
	ejournald_sup:stop(Id).

%% ----------------------------------------------------------------------------------------------------
%% -- API for ejournald_reader
-spec start_reader() -> {ok, pid()} | {error, any()}.
start_reader() ->
	ejournald_sup:start(?READER, []).

-spec start_reader( term() ) -> {ok, pid()} | {error, any()}.
start_reader(Name) ->
	ejournald_sup:start(?READER, Name, []).

-spec stop_reader( term() ) -> ok | {error, any()}.
stop_reader(Id) ->
	ejournald_sup:stop(Id).

%% ----------------------------------------------------------------------------------------------------
%% -- API for retrieving logs
-spec get_logs( [log_options()] ) -> [ log_message() ] | {error, any()}.
get_logs(Options) ->
	get_logs(?READER, Options).

-spec get_logs(id(), [log_options()] ) -> [ log_message() ] | {error, any()}.
get_logs(Id, Options) ->
	case check_options(Options) of
		{Error, Reason} -> {Error, Reason};
		ok 				-> gen_server:call(Id, {evaluate, Options})
	end.
	

-spec log_notify(sink(), [notify_options()] ) -> {ok, pid()} | {error, any()}.
log_notify(Sink, Options) ->
	log_notify(?READER, Sink, Options).

-spec log_notify(id(), sink(), [notify_options()] ) -> {ok, pid()} | {error, any()}.
log_notify(Id, Sink, Options) ->
	case check_options(Options) of
		{Error, Reason} -> {Error, Reason};
		ok 				-> evaluate_options_notify(Id, Sink, Options)
	end.

%% ----------------------------------------------------------------------------------------------------
%% -- helpers
%% @private
evaluate_options_notify(Id, Sink, Options) -> 
	case Sink of
		undefined -> erlang:error(badarg, {error, no_sink});
		_Sink -> ok
	end,
	Cursor = gen_server:call(Id, last_entry_cursor),
	Pid = spawn(?MODULE, log_notify_worker, [Id, Sink, [ {last_entry_cursor, Cursor} | Options ]]),
	ok = gen_server:call(Id, {register_notifier, Pid}),
	{ok, Pid}.

%% @private
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

%% @private
evaluate_sink(_Sink, []) -> ok;
evaluate_sink(Sink, [ Msg = {_Timestamp, _Priority, _Data} | Result]) when is_pid(Sink) ->
	Sink ! Msg,
	evaluate_sink(Sink, Result);
evaluate_sink(Sink, [ Msg = {_Timestamp, _Priority, _Log} | Result]) when is_function(Sink,1) ->
	catch(Sink(Msg)),
	evaluate_sink(Sink, Result);
evaluate_sink(Sink, [ _ | Result]) when is_pid(Sink);is_function(Sink,1) ->
	evaluate_sink(Sink, Result);
evaluate_sink(_Sink, _Result) ->
	erlang:error(badarg).

%% @private
check_options([]) ->
	ok;
check_options([{direction, Dir} | RestOpts]) when Dir =:=bot;Dir =:= top ->
	check_options(RestOpts);
check_options([{since, {{Y,M,D}, {H,Min,S}}} | RestOpts]) 
	when is_number(Y),is_number(M),is_number(D),is_number(H),is_number(Min),is_number(S) ->
	check_options(RestOpts);
check_options([{until, {{Y,M,D}, {H,Min,S}}} | RestOpts]) 
	when is_number(Y),is_number(M),is_number(D),is_number(H),is_number(Min),is_number(S) ->
	check_options(RestOpts);
check_options([{at_most, AtMost} | RestOpts]) when is_number(AtMost) -> 
	check_options(RestOpts);
check_options([{log_level, LogLevel} | RestOpts]) when is_atom(LogLevel) -> 
 	case lists:member(LogLevel, [emergency, alert, critical, error, warning, notice, info, debug]) of
 		true 	-> check_options(RestOpts);
 		false 	-> {badarg, {invalid_log_level, LogLevel}}
 	end;
check_options([{message, Message} | RestOpts]) when Message=:=true;Message=:=false -> 
	check_options(RestOpts);
check_options([ Arg | _RestOpts]) -> 
	{badarg, Arg}.

