% Copyright 2010-2011, Travelping GmbH <info@travelping.com>

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

%% External API
-export([sendv/1, stream_fd/3]).

-on_load(load_nif/0).

sendv(_X) ->
    "NIF library not loaded".

stream_fd(_A, _B, _C) -> 
    "NIF library not loaded".

load_nif() ->
    Dir = "priv",
    PrivDir = case code:priv_dir(ejournald) of   % check existence of priv folder 
        {error, _} -> Dir; 
        X -> X
    end,
    Lib = filename:join(PrivDir, "journald_api"),   % create priv path so journald_api.so 
    erlang:load_nif(Lib, 0).						% load NIF 


