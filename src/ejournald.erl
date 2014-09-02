-module(ejournald).
-behaviour(application).

-export([start/2, stop/1]).
-export([start_io/1, start_io/2, stop_io/1]).

%% ----------------------------------------------------------------------------------------------------
%% -- application callbacks
start(_Type, _Args) ->
	ejournald_io_sup:start_link().

stop(_State) ->
	ok.

%% ----------------------------------------------------------------------------------------------------
%% -- interface for ejournald_io_server
start_io(Options) ->
	ejournald_io_sup:start_io_server(Options).

start_io(Name, Options) ->
	ejournald_io_sup:start_io_server(Name, Options).

stop_io(Id) ->
	ejournald_io_sup:stop_io_server(Id).
