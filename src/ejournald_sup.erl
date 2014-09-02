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
    {ok, Pid} = supervisor:start_child(?MODULE, ChildSpec),
    register(Name, Pid),
    {ok, Pid}.

stop(Name) when is_atom(Name) ->
    case whereis(Name) of
        Pid when is_pid(Pid) -> 
        	supervisor:terminate_child(?MODULE, Name),
    		supervisor:delete_child(?MODULE, Name);
        _ -> 
        	ok
    end;
stop(Pid) when is_pid(Pid) -> 
    supervisor:terminate_child(?MODULE, Pid);
stop(Name) ->
    supervisor:terminate_child(?MODULE, Name),
    supervisor:delete_child(?MODULE, Name).

%% ----------------------------------------------------------------------------------------------------
%% -- supervisor callbacks
init(_Arg) ->
    {ok, {{one_for_one, 0, 1}, []}}.

%% ----------------------------------------------------------------------------------------------------
%% -- helpers
child_spec(Mod, Options) ->
	child_spec(make_ref(), Mod, Options).
child_spec(Name, Mod, Options) ->
    {   Name,
        {Mod, start_link, [Options]},
        transient,
        infinity,
        worker,
        [Mod]
    }.
