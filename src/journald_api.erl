% Copyright 2010-2013, Travelping GmbH <info@travelping.com>

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

-module(journald_api).

-define(SYSLOG_ID, "SYSLOG_IDENTIFIER").

%% External API
-export([sendv/1, stream_fd/3, write_fd/2, close_fd/1,
        open/0, open_directory/1, close/1, next/1, 
        previous/1, get_data/2, add_match/2,  
        add_disjunction/1, flush_matches/1, seek_head/1, seek_tail/1,  
        get_cursor/1, test_cursor/2, seek_cursor/2, query_unique/2,
        enumerate_unique/1, restart_unique/1, open_notifier/2,
        enumerate_data/1, restart_data/1, close_notifier/1,
        get_realtime_usec/1, seek_realtime_usec/2]).

-on_load(load_nif/0).

-type value() :: number() | atom() | iolist().
-spec sendv([{iolist(),value()}]) -> any().

-define(nif_stub,nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

%% ----------------------------------------------------------------------------------
%% -- low level journald API
sendv(Args) ->
    error_wrapper( fun sendv_nif/1, [list_conversion(Args)] ).

stream_fd(Id, Prio, LvlPrefix) -> 
    error_wrapper( fun stream_fd_nif/3, [Id, Prio, LvlPrefix] ).

write_fd(Fd, Msg) ->
    error_wrapper( fun write_fd_nif/2, [Fd, Msg] ).

close_fd(Fd) ->
    error_wrapper( fun close_fd_nif/1, [Fd] ).

open() ->
    error_wrapper( fun open_nif/0, [] ).

open_directory(Dir) ->
    error_wrapper( fun open_directory_nif/1, [Dir] ).

close(Fd) ->    
    error_wrapper( fun close_nif/1, [Fd] ).

next(Fd) ->
    error_wrapper( fun next_nif/1, [Fd] ).

previous(Fd) -> 
    error_wrapper( fun previous_nif/1, [Fd] ).

get_data(Fd, Field) -> 
    error_wrapper( fun get_data_nif/2, [Fd, Field] ).

add_match(Fd, Field) -> 
    error_wrapper( fun add_match_nif/2, [Fd, Field] ).

add_disjunction(Fd) -> 
    error_wrapper( fun add_disjunction_nif/1, [Fd] ).

flush_matches(Fd) -> 
    error_wrapper( fun flush_matches_nif/1, [Fd] ).

seek_head(Fd) -> 
    error_wrapper( fun seek_head_nif/1, [Fd] ).

seek_tail(Fd) -> 
    error_wrapper( fun seek_tail_nif/1, [Fd] ).

get_cursor(Fd) ->
    error_wrapper( fun get_cursor_nif/1, [Fd] ).

test_cursor(Fd, Cursor) ->
    error_wrapper( fun test_cursor_nif/2, [Fd, Cursor] ).

seek_cursor(Fd, Cursor) ->
    error_wrapper( fun seek_cursor_nif/2, [Fd, Cursor] ).

query_unique(Fd, Field) ->
    error_wrapper( fun query_unique_nif/2, [Fd, Field] ).

enumerate_unique(Fd) ->
    error_wrapper( fun enumerate_unique_nif/1, [Fd] ).

restart_unique(Fd) ->
    error_wrapper( fun restart_unique_nif/1, [Fd] ).

open_notifier(Fd, Pid) ->
    error_wrapper( fun open_notifier_nif/2, [Fd, Pid] ).

enumerate_data(Fd) ->
    error_wrapper( fun enumerate_data_nif/1, [Fd] ).

restart_data(Fd) ->
    error_wrapper( fun restart_data_nif/1, [Fd] ).

close_notifier(Fd) ->
    error_wrapper( fun close_notifier_nif/1, [Fd] ).

get_realtime_usec(Fd) ->
    error_wrapper( fun get_realtime_usec_nif/1, [Fd] ).

seek_realtime_usec(Fd, Usec) ->
    error_wrapper( fun seek_realtime_usec_nif/2, [Fd, Usec] ).


%% ----------------------------------------------------
%% --  NIF dummies
sendv_nif(_Args) -> ?nif_stub.
stream_fd_nif(_A, _B, _C) -> ?nif_stub.
write_fd_nif(_Fd, _Msg) -> ?nif_stub.
close_fd_nif(_Fd) -> ?nif_stub.
open_nif() -> ?nif_stub.
open_directory_nif(_Arg) -> ?nif_stub.
close_nif(_Arg) -> ?nif_stub.    
next_nif(_Arg) -> ?nif_stub.
previous_nif(_Arg) -> ?nif_stub. 
get_data_nif(_Arg1, _Arg2) -> ?nif_stub. 
add_match_nif(_Arg1, _Arg2) -> ?nif_stub. 
add_disjunction_nif(_Arg) -> ?nif_stub. 
flush_matches_nif(_Arg) -> ?nif_stub. 
seek_head_nif(_Arg) -> ?nif_stub. 
seek_tail_nif(_Arg) -> ?nif_stub. 
get_cursor_nif(_Arg) -> ?nif_stub.
test_cursor_nif(_Arg1, _Arg2) -> ?nif_stub.
seek_cursor_nif(_Arg1, _Arg2) -> ?nif_stub.
query_unique_nif(_Arg1, _Arg2) -> ?nif_stub.
enumerate_unique_nif(_Arg) -> ?nif_stub.
restart_unique_nif(_Arg) -> ?nif_stub.
open_notifier_nif(_Arg1, _Arg2) -> ?nif_stub.
enumerate_data_nif(_Arg) -> ?nif_stub.
restart_data_nif(_Arg) -> ?nif_stub.
close_notifier_nif(_Arg) -> ?nif_stub.
get_realtime_usec_nif(_Journal) -> ?nif_stub.
seek_realtime_usec_nif(_Journal, _Usec) -> ?nif_stub.


%% -----------------------------------------------------------------
%% -- helpers
error_wrapper(Fun, Args) ->
    case erlang:apply(Fun, Args) of
        badarg ->
            erlang:error(badarg, Args);
        Result ->
            Result
    end.
 
list_conversion([])    -> [];
list_conversion([{E,V}|T])  ->
    [[E, $=, to_list(V)] | list_conversion(T)]; 
list_conversion([_|T]) ->                          % skip bad argument
    list_conversion(T);
list_conversion(_) -> [].                         

to_list(V) when is_integer(V) -> integer_to_list(V);
to_list(V) when is_float(V)   -> float_to_list(V);
to_list(V) when is_atom(V)    -> atom_to_binary(V, utf8);
to_list(V) when is_pid(V)     -> pid_to_list(V);
to_list(V) -> V. 

load_nif() ->
    Dir = "priv",
    PrivDir = case code:priv_dir(ejournald) of      % check existence of priv folder 
        {error, _} -> Dir; 
        X -> X
    end,
    Lib = filename:join(PrivDir, "journald_api"),   % create priv path so journald_api.so 
    erlang:load_nif(Lib, 0).                        % load NIF 
