%% @author tmuszbek
%% @doc @todo Add description to youtube_player_fsm_test.


-module(youtube_player_fsm_test).
-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, lager_transform}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

setup() ->
	meck:new(python_lib),
	meck:expect(python_lib, start_python, fun spawn_mock_python/1),
	meck:expect(python_lib, stop_player, fun kill_mock_python/1),
	ok.

cleanup(_) ->
	gen_fsm:sync_send_all_state_event(youtube_player_fsm, stop),
	gen_server:call(python_server, stop),
	meck:unload(python_lib).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

fsm_starts_first_test() ->
	setup(),
	fsm_before_server(),
	server_after_fsm(),
	cleanup(ok).
	

fsm_before_server() ->
	youtube_player_fsm:start_link(),
	?assertEqual(down, get_state()).

server_after_fsm() ->
	python_server:start_link(),
	?assertEqual(idle, get_state()).


server_starts_first_test() ->
	setup(),
	server_before_fsm(),
	fsm_after_server(),
	cleanup(ok).


server_before_fsm() ->
	python_server:start_link().

fsm_after_server() ->
	youtube_player_fsm:start_link(),
	?assertEqual(idle, get_state()).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

spawn_mock_python(_ServerID) ->
	Pid = spawn(fun() -> timer:sleep(2000) end),
	Pid.

kill_mock_python(Pid) ->
	exit(Pid, normal).

get_state() ->
	Status = sys:get_status(whereis(youtube_player_fsm)),
	{status, _Pid, _Behav, [_Call, running, _Parent, _, Contents]} = Status,
	[_Header, {data, Data}, _StateData] = Contents,
	[_, _, _, {"StateName", StateName}] = Data,
	StateName.
	
