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
        previous/1, get_data/2,    add_match/2, add_conjunction/1, 
        add_disjunction/1, flush_matches/1, seek_head/1, seek_tail/1,  
        get_cursor/1, test_cursor/2, seek_cursor/2, query_unique/2,
        enumerate_unique/1, restart_unique/1, open_notifier/2,
        enumerate_data/1, restart_data/1, close_notifier/1,
        get_realtime_usec/1, get_monotonic_usec/1,
        seek_realtime_usec/2, seek_monotonic_usec/3]).

-on_load(load_nif/0).

-type value() :: number() | atom() | iolist().
-spec sendv([{iolist(),value()}]) -> any().
 
sendv(Args) ->
    sendv_nif(list_conversion(Args)).

sendv_nif(_Args) ->
    "NIF library not loaded".

stream_fd(_A, _B, _C) -> 
    "NIF library not loaded".

write_fd(_Fd, _Msg) ->
    "NIF library not loaded".

close_fd(_Fd) ->
    "NIF library not loaded".

open() ->
    "NIF library not loaded".

open_directory(_Arg) ->
    "NIF library not loaded".

close(_Arg) ->    
    "NIF library not loaded".

next(_Arg) ->
    "NIF library not loaded".

previous(_Arg) -> 
    "NIF library not loaded".

get_data(_Arg1, _Arg2) -> 
     "NIF library not loaded".

add_match(_Arg1, _Arg2) -> 
     "NIF library not loaded".

add_disjunction(_Arg) -> 
     "NIF library not loaded".

add_conjunction(_Arg) -> 
     "NIF library not loaded".

flush_matches(_Arg) -> 
     "NIF library not loaded".

seek_head(_Arg) -> 
     "NIF library not loaded".

seek_tail(_Arg) -> 
   "NIF library not loaded".

get_cursor(_Arg) ->
    "NIF library not loaded".

test_cursor(_Arg1, _Arg2) ->
    "NIF library not loaded".

seek_cursor(_Arg1, _Arg2) ->
    "NIF library not loaded".

query_unique(_Arg1, _Arg2) ->
    "NIF library not loaded".

enumerate_unique(_Arg) ->
    "NIF library not loaded".

restart_unique(_Arg) ->
    "NIF library not loaded".

open_notifier(_Arg1, _Arg2) ->
    "NIF library not loaded".

enumerate_data(_Arg) ->
    "NIF library not loaded".

restart_data(_Arg) ->
    "NIF library not loaded".

close_notifier(_Arg) ->
    "NIF library not loaded".

get_realtime_usec(_Journal) ->
    "NIF library not loaded".

seek_realtime_usec(_Journal, _Usec) ->
    "NIF library not loaded".

get_monotonic_usec(_Journal) ->
    "NIF library not loaded".

seek_monotonic_usec(_Args1, _Arg2, _Arg3) ->
    "NIF library not loaded".

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
