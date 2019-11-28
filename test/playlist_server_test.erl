%% @author tmuszbek
%% @doc @todo Add description to youtube_player_fsm_test.


-module(playlist_server_test).
-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, lager_transform}]).

-define(VIDEO_LENGTH, 20).
-define(TEST_URL, << "test_url" >>).
-define(TEST_URL_NEW, << "test_url_new" >>).

-define(setup(F), {foreach, fun setup/0, fun cleanup/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

playlist_plays_in_order_test_() ->
	{"The playlist server accummulates videos and forwards them as the fsm is available.",
	 ?setup([fun first_video_plays/1,
			 fun first_video_finishes/1,
			 fun second_video_goes_to_list/1,
			 fun second_video_plays/1])}.

playlist_persists_on_failure_test_() ->
	{"The playlist server stays alive and keeps the playlist when the fsm dies.",
	 ?setup([fun no_fsm_playlist_stays_alive/1])}.


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

%% playlist_plays_in_order_test
first_video_plays(_) ->
	youtube_player_fsm:start_link(),
	playlist_server:start_link(),
	playlist_server:publish_video(?TEST_URL),
	State = get_state(),
	?_assertMatch({state, [], {video, ?TEST_URL, _Publisher}}, State).

first_video_finishes(_) ->
	youtube_player_fsm:start_link(),
	playlist_server:start_link(),
	playlist_server:publish_video(?TEST_URL),
	timer:sleep(?VIDEO_LENGTH + 10),
	State = get_state(),
	?_assertMatch({state, [], {video, [], undefined}}, State).

second_video_goes_to_list(_) ->
	youtube_player_fsm:start_link(),
	playlist_server:start_link(),
	playlist_server:publish_video(?TEST_URL),
	playlist_server:publish_video(?TEST_URL_NEW),
	State = get_state(),
	?_assertMatch({state,
				   [{video, ?TEST_URL_NEW, _PublisherNew}],
				   {video, ?TEST_URL, _Publisher}}, State).

second_video_plays(_) ->
	youtube_player_fsm:start_link(),
	playlist_server:start_link(),
	playlist_server:publish_video(?TEST_URL),
	playlist_server:publish_video(?TEST_URL_NEW),
	timer:sleep(?VIDEO_LENGTH),
	State = get_state(),
	?_assertMatch({state, [], {video, ?TEST_URL_NEW, _Publisher}}, State).


%% playlist_persists_on_failure_test
no_fsm_playlist_stays_alive(_) ->
	playlist_server:start_link(),
	playlist_server:publish_video(?TEST_URL),
	State = get_state(),
	?_assertMatch({state, [{video, ?TEST_URL, _Publisher}],
				   {video, [], undefined}}, State).
	

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

start_link_mock_fsm() ->
	_Tid = ets:new(mock_fsm_state, [named_table, public]),
	ok.


new_video_mock_fsm(Url) ->
	case mock_is_server_alive() of
		true ->
			mock_new_video_event(Url);
		false ->
			video_refused
	end.

mock_is_server_alive() ->
	case ets:info(mock_fsm_state) of
		undefined ->
			false;
		_Info ->
			true
	end.

mock_new_video_event(Url) ->
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
	case mock_is_server_alive() of
		true ->
			mock_kill_fsm_action();
		false ->
			ok
	end.

mock_kill_fsm_action() ->
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
