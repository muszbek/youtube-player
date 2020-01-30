%% @author tmuszbek
%% @doc Unit tests concerning the functionality of the playlist server, with mocked fsm.


-module(playlist_server_test).
-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, lager_transform}]).

-define(VIDEO_LENGTH, 20).
-define(TEST_URL, << "test_url" >>).
-define(TEST_URL_NEW, << "test_url_new" >>).
-define(TEST_URL_WRONG, << "test_url_wrong" >>).	%% this is an url that youtube does not recognize as video

-define(TEST_VIDEO, {video, ?TEST_URL, _Publisher, <<"test_title">>, <<"test_dur">>, 1}).
-define(TEST_VIDEO_NEW, {video, ?TEST_URL_NEW, _PublisherNew, <<"test_title_new">>, <<"test_dur_new">>, 2}).
-define(NO_VIDEO, {video, undefined, undefined, undefined, undefined, undefined}).

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
	 ?setup([fun no_fsm_playlist_stays_alive/1,
			 fun fsm_revives_empty_playlist_nothing_happens/1,
			 fun fsm_revives_current_video_replays/1,
			 fun fsm_revives_no_current_video_playlist_plays/1])}.

playlist_rejects_wrong_urls_test_() ->
	{"Wrong urls get rejected and do not enter the playlist.",
	 ?setup([fun erronous_url_does_not_play/1,
			 fun erronous_url_does_not_enter_playlist/1])}.

remove_from_playlist_test_() ->
	{"Remove video from playlist only when publisher is matching.",
	 ?setup([fun remove_from_playlist_when_publisher_matching/1,
			 fun leave_in_playlist_when_publisher_not_matching/1,
			 fun leave_in_playlist_when_id_not_found/1])}.

remove_current_video_test_() ->
	{"Stop playing current video and go to the next in playlist if publisher is matching.",
	 ?setup([fun remove_current_video_when_publisher_matching/1,
			 fun leave_current_video_when_publisher_not_matching/1,
			 fun leave_current_video_when_id_not_matching/1,
			 fun do_not_crash_when_trying_to_remove_nothing/1])}.


%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

setup() ->
	meck:new(youtube_player_fsm),
	meck:expect(youtube_player_fsm, start_link, fun start_link_mock_fsm/0),
	meck:expect(youtube_player_fsm, new_video, fun new_video_mock_fsm/1),
	
	meck:new(python_server),
	meck:expect(python_server, get_video_details, fun mock_get_video_details/1),
	meck:expect(python_server, stop_video, fun mock_stop_video/0),
	ok.

cleanup(_) ->
	case whereis(playlist_server) == undefined of
		true -> ok;
		false -> gen_server:call(playlist_server, stop)
	end,
	
	mock_kill_fsm(),
	meck:unload(youtube_player_fsm),
	meck:unload(python_server).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

%% playlist_plays_in_order_test
first_video_plays(_) ->
	playlist_server:start_link(),
	youtube_player_fsm:start_link(),
	playlist_server:publish_video(?TEST_URL),
	State = get_state(),
	?_assertMatch({state, [], ?TEST_VIDEO}, State).

first_video_finishes(_) ->
	playlist_server:start_link(),
	youtube_player_fsm:start_link(),
	playlist_server:publish_video(?TEST_URL),
	timer:sleep(?VIDEO_LENGTH + 10),
	State = get_state(),
	?_assertMatch({state, [], ?NO_VIDEO}, State).

second_video_goes_to_list(_) ->
	playlist_server:start_link(),
	youtube_player_fsm:start_link(),
	playlist_server:publish_video(?TEST_URL),
	playlist_server:publish_video(?TEST_URL_NEW),
	State = get_state(),
	?_assertMatch({state,
				   [?TEST_VIDEO_NEW],
				   ?TEST_VIDEO}, State).

second_video_plays(_) ->
	playlist_server:start_link(),
	youtube_player_fsm:start_link(),
	playlist_server:publish_video(?TEST_URL),
	playlist_server:publish_video(?TEST_URL_NEW),
	timer:sleep(?VIDEO_LENGTH + 10),
	State = get_state(),
	?_assertMatch({state, [], ?TEST_VIDEO_NEW}, State).


%% playlist_persists_on_failure_test
no_fsm_playlist_stays_alive(_) ->
	playlist_server:start_link(),
	playlist_server:publish_video(?TEST_URL),
	State = get_state(),
	?_assertMatch({state, [?TEST_VIDEO],
				   ?NO_VIDEO}, State).

fsm_revives_empty_playlist_nothing_happens(_) ->
	playlist_server:start_link(),
	youtube_player_fsm:start_link(),
	State = get_state(),
	?_assertMatch({state, [], ?NO_VIDEO}, State).

fsm_revives_current_video_replays(_) ->
	playlist_server:start_link(),
	youtube_player_fsm:start_link(),
	playlist_server:publish_video(?TEST_URL),
	mock_kill_fsm(),
	youtube_player_fsm:start_link(),
	State = get_state(),
	?_assertMatch({state, [], ?TEST_VIDEO}, State).

fsm_revives_no_current_video_playlist_plays(_) ->
	playlist_server:start_link(),
	playlist_server:publish_video(?TEST_URL),
	youtube_player_fsm:start_link(),
	State = get_state(),
	?_assertMatch({state, [], ?TEST_VIDEO}, State).


%% playlist_rejects_wrong_urls_test
erronous_url_does_not_play(_) ->
	playlist_server:start_link(),
	youtube_player_fsm:start_link(),
	playlist_server:publish_video(?TEST_URL_WRONG),
	State = get_state(),
	?_assertMatch({state, [], ?NO_VIDEO}, State).

erronous_url_does_not_enter_playlist(_) ->
	playlist_server:start_link(),
	youtube_player_fsm:start_link(),
	playlist_server:publish_video(?TEST_URL),
	playlist_server:publish_video(?TEST_URL_WRONG),
	State = get_state(),
	?_assertMatch({state, [], ?TEST_VIDEO}, State).


%% remove_from_playlist_test
remove_from_playlist_when_publisher_matching(_) ->
	playlist_server:start_link(),
	youtube_player_fsm:start_link(),
	playlist_server:publish_video(?TEST_URL, self()),
	playlist_server:publish_video(?TEST_URL_NEW, self()),
	playlist_server:remove_video(2, self()),
	State = get_state(),
	?_assertMatch({state, [], ?TEST_VIDEO}, State).

leave_in_playlist_when_publisher_not_matching(_) ->
	playlist_server:start_link(),
	youtube_player_fsm:start_link(),
	playlist_server:publish_video(?TEST_URL, self()),
	playlist_server:publish_video(?TEST_URL_NEW, not_me),
	playlist_server:remove_video(2, self()),
	State = get_state(),
	?_assertMatch({state, [?TEST_VIDEO_NEW], ?TEST_VIDEO}, State).

leave_in_playlist_when_id_not_found(_) ->
	playlist_server:start_link(),
	youtube_player_fsm:start_link(),
	playlist_server:publish_video(?TEST_URL, self()),
	playlist_server:publish_video(?TEST_URL_NEW, self()),
	playlist_server:remove_video(3, self()),
	State = get_state(),
	?_assertMatch({state, [?TEST_VIDEO_NEW], ?TEST_VIDEO}, State).


%% remove_current_video_test
remove_current_video_when_publisher_matching(_) ->
	playlist_server:start_link(),
	youtube_player_fsm:start_link(),
	playlist_server:publish_video(?TEST_URL, self()),
	playlist_server:publish_video(?TEST_URL_NEW, self()),
	playlist_server:remove_video(1, self()),
	State = get_state(),
	?_assertMatch({state, [], ?TEST_VIDEO_NEW}, State).

leave_current_video_when_publisher_not_matching(_) ->
	playlist_server:start_link(),
	youtube_player_fsm:start_link(),
	playlist_server:publish_video(?TEST_URL, not_me),
	playlist_server:publish_video(?TEST_URL_NEW, self()),
	playlist_server:remove_video(1, self()),
	State = get_state(),
	?_assertMatch({state, [?TEST_VIDEO_NEW], ?TEST_VIDEO}, State).

leave_current_video_when_id_not_matching(_) ->
	playlist_server:start_link(),
	youtube_player_fsm:start_link(),
	playlist_server:publish_video(?TEST_URL, self()),
	playlist_server:publish_video(?TEST_URL_NEW, self()),
	playlist_server:remove_video(3, self()),
	State = get_state(),
	?_assertMatch({state, [?TEST_VIDEO_NEW], ?TEST_VIDEO}, State).

do_not_crash_when_trying_to_remove_nothing(_) ->
	playlist_server:start_link(),
	youtube_player_fsm:start_link(),
	playlist_server:remove_video(1, self()),
	State = get_state(),
	?_assertMatch({state, [], ?NO_VIDEO}, State).


%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

start_link_mock_fsm() ->
	_Tid = ets:new(mock_fsm_state, [named_table, public]),
	playlist_server:replay_video(),
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


mock_stop_video() ->
	try ets:delete(mock_fsm_state, current_video) of
		true -> ok
	catch
		error:badarg -> ok
	end,
	playlist_server:next_video().


mock_get_video_details(?TEST_URL) ->
	{<<"test_title">>, <<"test_dur">>, 1};

mock_get_video_details(?TEST_URL_NEW) ->
	{<<"test_title_new">>, <<"test_dur_new">>, 2};

mock_get_video_details(?TEST_URL_WRONG) ->
	wrong_url_error.


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
