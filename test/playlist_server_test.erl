%% @author tmuszbek
%% @doc @todo Add description to youtube_player_fsm_test.


-module(playlist_server_test).
-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, lager_transform}]).

-define(VIDEO_LENGTH, 10).
-define(TEST_URL, << "test_url" >>).
-define(TEST_URL_NEW, << "test_url_new" >>).

-define(setup(F), {foreach, fun setup/0, fun cleanup/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

playlist_plays_in_order_test_() ->
	{"The playlist server accummulates videos and forwards them as the fsm is available.",
	 ?setup([fun first_video_plays/1,
			 fun second_video_goes_to_list/1])}.


%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

setup() ->
	meck:new(youtube_player_fsm),
	meck:expect(youtube_player_fsm, start_link, fun start_link_mock_fsm/0),
	meck:expect(youtube_player_fsm, new_video, fun new_video_mock_fsm/1),
	ok.

cleanup(_) ->
	case whereis(playlist_server) == undefined of
		true -> ok;
		false -> gen_server:call(playlist_server, stop)
	end,
	
	mock_kill_fsm(),
	meck:unload(youtube_player_fsm).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

%% fsm_starts_first_test
first_video_plays(_) ->
	youtube_player_fsm:start_link(),
	playlist_server:start_link(),
	playlist_server:publish_video(?TEST_URL),
	State = get_state(),
	?_assertMatch({state, [], {video, ?TEST_URL, _Publisher}}, State).

second_video_goes_to_list(_) ->
	youtube_player_fsm:start_link(),
	playlist_server:start_link(),
	playlist_server:publish_video(?TEST_URL),
	playlist_server:publish_video(?TEST_URL_NEW),
	State = get_state(),
	?_assertMatch({state,
				   [{video, ?TEST_URL_NEW, _PublisherNew}],
				   {video, ?TEST_URL, _Publisher}}, State).
	

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

start_link_mock_fsm() ->
	_Tid = ets:new(mock_fsm_state, [named_table, public]),
	ok.

new_video_mock_fsm(Url) ->
	case ets:lookup(mock_fsm_state, current_video) of
		[] ->
			Pid = spawn(fun() -> mock_play_video(Url) end),
			ets:insert(mock_fsm_state, {current_video_pid, Pid}),
			video_accepted;
		[_OtherUrl] ->
			video_refused
	end.

mock_play_video(Url) ->
	ets:insert(mock_fsm_state, {current_video, Url}),
	timer:sleep(?VIDEO_LENGTH),
	try ets:delete(mock_fsm_state, current_video) of
		true -> ok
	catch
		error:badarg -> ok
	end,
	playlist_server:next_video().

mock_kill_fsm() ->
	case ets:lookup(mock_fsm_state, current_video_pid) of
		[] ->
			ok;
		[{current_video_pid, Pid}] ->
			exit(Pid, cleanup_mock_video_process)
	end,
	ets:delete(mock_fsm_state).


get_state() ->
	Status = sys:get_status(whereis(playlist_server)),
	{status, _Pid, _Behav, [_Call, running, _Parent, _, Contents]} = Status,
	[_Header, {data, _Data}, StateData] = Contents,
	{data, [{"State", State}]} = StateData,
	{state, _Playlist, _CurrentVideo} = State.
