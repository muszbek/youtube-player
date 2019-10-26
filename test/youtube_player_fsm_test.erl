%% @author tmuszbek
%% @doc @todo Add description to youtube_player_fsm_test.


-module(youtube_player_fsm_test).
-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, lager_transform}]).

-define(TEST_URL, << "test_url" >>).
-define(TEST_URL_NEW, << "test_url_new" >>).

-define(setup(F), {foreach, fun setup/0, fun cleanup/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

fsm_starts_first_test_() ->
	{"The fsm waits in down state until the python server is up at startup.",
	 ?setup([fun fsm_before_server/1,
			 fun server_after_fsm/1])}.

server_starts_first_test_() ->
	{"The fsm starts up at idle state if the server is already up.",
	 ?setup([fun fsm_after_server/1])}.

server_dies_test_() ->
	{"The fsm goes to down state if server dies, and comes back to idle when server revives.",
	 ?setup([fun server_down/1,
			 fun server_revives/1])}.

video_playing_test_() ->
	{"The fsm is in playing state when the server is playing. Url of current video matches.",
	 ?setup([fun video_starts/1,
			 fun new_video_playing/1,
			 fun video_finishes/1])}.

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
fsm_before_server(_) ->
	youtube_player_fsm:start_link(),
	?_assertEqual(down, get_state()).

server_after_fsm(_) ->
	youtube_player_fsm:start_link(),
	python_server:start_link(),
	?_assertEqual(idle, get_state()).


%% server_starts_first_test
fsm_after_server(_) ->
	python_server:start_link(),
	youtube_player_fsm:start_link(),
	?_assertEqual(idle, get_state()).


%% server_dies_test
server_down(_) ->
	youtube_player_fsm:start_link(),
	python_server:start_link(),
	gen_server:call(python_server, stop),
	timer:sleep(10),
	?_assertEqual(down, get_state()).

server_revives(_) ->
	youtube_player_fsm:start_link(),
	python_server:start_link(),
	gen_server:call(python_server, stop),
	timer:sleep(10),
	python_server:start_link(),
	?_assertEqual(idle, get_state()).


%% video_playing_test
video_starts(_) ->
	youtube_player_fsm:start_link(),
	python_server:start_link(),
	python_server:play_video(?TEST_URL),
	timer:sleep(10),
	?_assertEqual(playing, get_state()).

new_video_playing(_) ->
	youtube_player_fsm:start_link(),
	python_server:start_link(),
	python_server:play_video(?TEST_URL),
	timer:sleep(10),
	python_server:play_video(?TEST_URL_NEW),
	timer:sleep(10),
	[?_assertEqual(playing, get_state()),
	 ?_assertEqual(?TEST_URL_NEW, get_current_video())].

video_finishes(_) ->
	youtube_player_fsm:start_link(),
	python_server:start_link(),
	python_server:play_video(?TEST_URL),
	gen_server:cast(python_server, finished),
	timer:sleep(10),
	[?_assertEqual(idle, get_state()),
	 ?_assertEqual(<< >>, get_current_video())].

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

get_current_video() ->
	Status = sys:get_status(whereis(youtube_player_fsm)),
	{status, _Pid, _Behav, [_Call, running, _Parent, _, Contents]} = Status,
	[_Header, _RunData, {data, Data}] = Contents,
	[{"StateData", State}] = Data,
	{state, CurrentUrl, _Ref} = State,
	CurrentUrl.
