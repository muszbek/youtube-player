%%%-------------------------------------------------------------------
%% @doc youtube_player top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(youtube_player_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	PlayerFSM = #{
				  id =>youtube_player_fsm,
				  start => {youtube_player_fsm, start_link, []},
				  restart => permanent,
				  shutdown => 2000,
				  type => worker,
				  modules => [python_server]
				 },
	
	PythonServer = #{
					 id => python_server,
					 start => {python_server, start_link, []},
					 restart => permanent,
					 shutdown => 2000,
					 type => worker,
					 modules => [python_server]
					 },
	
	PlaylistServer = #{
					 id => playlist_server,
					 start => {playlist_server, start_link, []},
					 restart => permanent,
					 shutdown => 2000,
					 type => worker,
					 modules => [playlist_server]
					 },
	
    {ok, {{one_for_one, 5, 60}, [PlayerFSM, PythonServer, PlaylistServer]}}.

%%====================================================================
%% Internal functions
%%====================================================================
