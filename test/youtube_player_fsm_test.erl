%% @author tmuszbek
%% @doc @todo Add description to youtube_player_fsm_test.


-module(youtube_player_fsm_test).
-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, lager_transform}]).

-define(TEST_URL, << "test_url" >>).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%% fixtures and test generators do not work, they do not execute linearly

fsm_starts_first_test() ->
	setup(),
	fsm_before_server(),
	server_after_fsm(),
	cleanup(ok).

server_starts_first_test() ->
	setup(),
	server_before_fsm(),
	fsm_after_server(),
	cleanup(ok).

server_dies_test() ->
	setup(),
	server_down(),
	server_revives(),
	cleanup(ok).

video_playing_test() ->
	setup(),
	video_starts(),
	video_finishes(),
	cleanup(ok).

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

setup() ->
	meck:new(python_lib),
	meck:expect(python_lib, start_python, fun spawn_mock_python/1),
	meck:expect(python_lib, play_video, fun play_mock_python/2),
	meck:expect(python_lib, stop_player, fun kill_mock_python/1),
	ok.

cleanup(_) ->
	case whereis(youtube_player_fsm) == undefined of
		true -> ok;
		false -> gen_fsm:send_event(youtube_player_fsm, stop)
	end,
	
	case whereis(python_server) == undefined of
		true -> ok;
		false -> gen_server:call(python_server, stop)
	end,
	
	meck:unload(python_lib).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

%% fsm_starts_first_test
fsm_before_server() ->
	youtube_player_fsm:start_link(),
	?assertEqual(down, get_state()).

server_after_fsm() ->
	python_server:start_link(),
	?assertEqual(idle, get_state()).


%% server_starts_first_test
server_before_fsm() ->
	python_server:start_link().

fsm_after_server() ->
	youtube_player_fsm:start_link(),
	?assertEqual(idle, get_state()).

%% server_dies_test
server_down() ->
	youtube_player_fsm:start_link(),
	python_server:start_link(),
	gen_server:call(python_server, stop),
	timer:sleep(10),
	?assertEqual(down, get_state()).

server_revives() ->
	python_server:start_link(),
	?assertEqual(idle, get_state()).

%% video_playing_test
video_starts() ->
	youtube_player_fsm:start_link(),
	python_server:start_link(),
	python_server:play_video(?TEST_URL),
	?assertEqual(playing, get_state()).

video_finishes() ->
	gen_server:cast(python_server, finished),
	timer:sleep(10),
	?assertEqual(idle, get_state()).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

spawn_mock_python(_ServerID) ->
	Pid = spawn(fun() -> timer:sleep(2000) end),
	Pid.

play_mock_python(_Pid, _Url) ->
	ok.

kill_mock_python(Pid) ->
	exit(Pid, normal).

get_state() ->
	Status = sys:get_status(whereis(youtube_player_fsm)),
	{status, _Pid, _Behav, [_Call, running, _Parent, _, Contents]} = Status,
	[_Header, {data, Data}, _StateData] = Contents,
	[_, _, _, {"StateName", StateName}] = Data,
	StateName.
	
