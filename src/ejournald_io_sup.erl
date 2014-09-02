-module(ejournald_io_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/0,
		 start_io_server/1,
		 start_io_server/2,
		 stop_io_server/1
		]).

%% ----------------------------------------------------------------------------------------------------
%% -- Public API
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc start an ejournald_io_server process
start_io_server(Options) ->
    ChildSpec = generate_child_spec(Options),
    supervisor:start_child(?MODULE, ChildSpec).

%% @doc start a named ejournald_io_server process
start_io_server(Name, Options) ->
    ChildSpec = generate_child_spec(Name, Options),
    {ok, Pid} = supervisor:start_child(?MODULE, ChildSpec),
    register(Name, Pid),
    {ok, Pid}.

%% @doc stop a client process
stop_io_server(Name) when is_atom(Name) ->
    case whereis(Name) of
        Pid when is_pid(Pid) -> 
        	supervisor:terminate_child(?MODULE, Name),
    		supervisor:delete_child(?MODULE, Name);
        _ -> 
        	ok
    end;
stop_io_server(Pid) when is_pid(Pid) -> 
    supervisor:terminate_child(?MODULE, Pid);
stop_io_server(Name) ->
    supervisor:terminate_child(?MODULE, Name),
    supervisor:delete_child(?MODULE, Name).

%% ----------------------------------------------------------------------------------------------------
%% -- supervisor callbacks
init(_Arg) ->
    {ok, {{one_for_one, 0, 1}, []}}.

%% ----------------------------------------------------------------------------------------------------
%% -- helpers
generate_child_spec(Options) ->
	generate_child_spec(make_ref(), Options).
generate_child_spec(Name, Options) ->
    {   Name,
        {ejournald_io_server, start_link, [Options]},
        transient,
        infinity,
        worker,
        [ejournald_io_server]
    }.
