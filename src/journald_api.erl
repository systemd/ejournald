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

-define(nif_stub,nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

%% ----------------------------------------------------------------------------------
%% -- low level journald API
-spec sendv([{iolist(),value()}]) -> any().
sendv(Args) ->
    sendv_nif(list_conversion(Args)).

sendv_nif(_Args) -> ?nif_stub.
stream_fd(_A, _B, _C) -> ?nif_stub.
write_fd(_Fd, _Msg) -> ?nif_stub.
close_fd(_Fd) -> ?nif_stub.
open() -> ?nif_stub.
open_directory(_Arg) -> ?nif_stub.
close(_Arg) -> ?nif_stub.    
next(_Arg) -> ?nif_stub.
previous(_Arg) -> ?nif_stub. 
get_data(_Arg1, _Arg2) -> ?nif_stub. 
add_match(_Arg1, _Arg2) -> ?nif_stub. 
add_disjunction(_Arg) -> ?nif_stub. 
flush_matches(_Arg) -> ?nif_stub. 
seek_head(_Arg) -> ?nif_stub. 
seek_tail(_Arg) -> ?nif_stub. 
get_cursor(_Arg) -> ?nif_stub.
test_cursor(_Arg1, _Arg2) -> ?nif_stub.
seek_cursor(_Arg1, _Arg2) -> ?nif_stub.
query_unique(_Arg1, _Arg2) -> ?nif_stub.
enumerate_unique(_Arg) -> ?nif_stub.
restart_unique(_Arg) -> ?nif_stub.
open_notifier(_Arg1, _Arg2) -> ?nif_stub.
enumerate_data(_Arg) -> ?nif_stub.
restart_data(_Arg) -> ?nif_stub.
close_notifier(_Arg) -> ?nif_stub.
get_realtime_usec(_Journal) -> ?nif_stub.
seek_realtime_usec(_Journal, _Usec) -> ?nif_stub.

%% -----------------------------------------------------------------
%% -- helpers
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
