%% @author tmuszbek
%% @doc Unit tests concerning the functionality of the playlist server, with mocked fsm.


-module(playlist_server_test).
-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, lager_transform}]).

-define(VIDEO_LENGTH, 20).
-define(TEST_URL, << "test_url" >>).
-define(TEST_URL_NEW, << "test_url_new" >>).

-define(TEST_VIDEO, {video, ?TEST_URL, _Publisher, <<"test_title">>, <<"test_dur">>}).
-define(TEST_VIDEO_NEW, {video, ?TEST_URL_NEW, _Publisher, <<"test_title_new">>, <<"test_dur_new">>}).
-define(NO_VIDEO, {video, [], undefined, undefined, undefined}).

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


%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

setup() ->
	meck:new(youtube_player_fsm),
	meck:expect(youtube_player_fsm, start_link, fun start_link_mock_fsm/0),
	meck:expect(youtube_player_fsm, new_video, fun new_video_mock_fsm/1),
	
	meck:new(python_server),
	meck:expect(python_server, get_video_details, fun mock_get_video_details/1),
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


mock_get_video_details(?TEST_URL) ->
	<<"{\"title\": \"test_title\", \"duration\": \"test_dur\"}">>;

mock_get_video_details(?TEST_URL_NEW) ->
	<<"{\"title\": \"test_title_new\", \"duration\": \"test_dur_new\"}">>.


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
