% Copyright 2010-2014, Travelping GmbH <info@travelping.com>

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

%% @private
-module(ejournald_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/0,
         start/2,
         start/3,
         stop/1
        ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ----------------------------------------------------------------------------------------------------
%% -- Public API for io server
start(Mod, Options) ->
    ChildSpec = child_spec(Mod, Options),
    supervisor:start_child(?MODULE, ChildSpec).

start(Mod, Name, Options) ->
    ChildSpec = child_spec(Name, Mod, Options),
    supervisor:start_child(?MODULE, ChildSpec).

stop(Name) when is_atom(Name) ->
    case whereis(Name) of
        Pid when is_pid(Pid) -> 
            ok = gen_server:call(Name, {terminate, normal}),
            supervisor:delete_child(?MODULE, Name);
        _ -> 
            ok
    end;
stop(Pid) when is_pid(Pid) -> 
    ok = gen_server:call(Pid, {terminate, normal});
stop(Name) ->
    ok = gen_server:call(Name, {terminate, normal}),
    supervisor:delete_child(?MODULE, Name).

%% ----------------------------------------------------------------------------------------------------
%% -- supervisor callbacks
init(_Arg) ->
    {ok, {{one_for_one, 0, 1}, []}}.

%% ----------------------------------------------------------------------------------------------------
%% -- helpers
child_spec(Mod, Options) ->
    child_spec(undefined, Mod, Options).
child_spec(Name, Mod, Options) ->
    {   Name,
        {Mod, start_link, [Name, Options]},
        transient,
        infinity,
        worker,
        [Mod]
    }.
