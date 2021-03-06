%% @author tmuszbek
%% @doc @todo Add description to playlist_server.


-module(playlist_server).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).

-include("playlist_server.hrl").
-include("python_server.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, next_video/0, replay_video/0, publish_video/1, publish_video/2, 
		 remove_video/2, get_playlist/0]).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

next_video() ->
	gen_server:cast(?SERVER, next_video).

replay_video() ->
	gen_server:call(?SERVER, replay_video).

publish_video(Url) ->
	publish_video(Url, self()).

publish_video(Url, Publisher) ->
	gen_server:call(?SERVER, {publish_video, Url, Publisher}, ?VIDEO_INFO_TIMEOUT).

remove_video(Id, Publisher) ->
	gen_server:call(?SERVER, {remove_video, Id, Publisher}).

get_playlist() ->
	gen_server:call(?SERVER, get_playlist).


%% ====================================================================
%% Behavioural functions
%% ====================================================================

-record(state, {playlist=[],
				current_video=#video{}}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
	lager:debug("Playlist server started"),
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call({publish_video, Url, Publisher}, _From, State=#state{playlist=Playlist}) ->
    lager:debug("Video published to playlist: ~p", [Url]),
	case python_server:get_video_details(Url) of
		Error when is_atom(Error) ->	%% wrong_url_error or timeout
			NewPlaylist = Playlist,
			Reply = Error;
		{Title, Duration, Id} ->
			Video = #video{url=Url, publisher=Publisher, title=Title, duration=Duration, id=Id},
			NewPlaylist = Playlist ++ [Video],	%% new video goes to back of playlist
			next_video(),
			Reply = ok
	end,
    {reply, Reply, State#state{playlist=NewPlaylist}};

handle_call({remove_video, Id, Publisher}, _From, 
			State=#state{current_video=#video{id=Id, publisher=Publisher, url=Url}}) ->
	lager:debug("Stopping current video ~p", [Url]),
	python_server:stop_video(),
	{reply, ok, State};

handle_call({remove_video, Id, _Publisher}, _From, 
			State=#state{current_video=#video{id=Id, publisher=_OtherPublisher, url=Url}}) ->
	lager:debug("Trying to stop current video ~p, have no right", [Url]),
	{reply, refused, State};

handle_call({remove_video, Id, Publisher}, _From, State=#state{playlist=Playlist}) ->
	ToRemove = [Video || Video=#video{id=ElementId} <- Playlist, ElementId == Id],
	case ToRemove of
		[] ->
			lager:debug("Trying to remove a video, but it is not in the playlist"),
			{reply, refused, State};
		[Video=#video{id=Id, publisher=Publisher, url=Url}] ->
			lager:debug("Removing video ~p from playlist", [Url]),
			NewPlaylist = lists:delete(Video, Playlist),
			{reply, ok, State#state{playlist=NewPlaylist}};
		[_Video=#video{id=Id, publisher=_OtherPublisher, url=Url}] ->
			lager:debug("Trying to remove video ~p from playlist, have no right", [Url]),
			{reply, refused, State}
	end;

handle_call(replay_video, _From, State=#state{current_video=_CurrVid=#video{url=undefined}}) ->
    lager:debug("Fsm revived, fetching video from playlist (if there is any)."),
	next_video(),
	Reply = ok,
    {reply, Reply, State};

handle_call(replay_video, _From, State=#state{current_video=_CurrVid=#video{url=Url}}) ->
    lager:debug("Replaying video: ~p", [Url]),
	youtube_player_fsm:new_video(Url),
	Reply = ok,
    {reply, Reply, State};

handle_call(get_playlist, _From, State=#state{current_video=CurrVid, playlist=Playlist}) ->
	lager:debug("Getting playlist"),
	Reply = {CurrVid, Playlist},
	{reply, Reply, State};

handle_call(stop, _From, State) ->
	lager:debug("Stopping playlist_server normally"),
	{stop, normal, shutdown_ok, State};

handle_call(Request, _From, State) ->
    lager:warning("!!! unexpected call received !!!"),
	Reply = {error, {unknown_request, Request}},
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(next_video, State=#state{playlist=[]}) ->
    lager:debug("Call for next video, playlist empty"),
    {noreply, State#state{current_video=#video{}}};

handle_cast(next_video, State=#state{playlist=Playlist}) ->
    lager:debug("Call for next video"),
	[NextVideo=#video{url=Url} | RemainingPlaylist] = Playlist,
	
	case youtube_player_fsm:new_video(Url) of
		video_accepted ->
			{noreply, State#state{playlist=RemainingPlaylist, current_video=NextVideo}};
		video_refused ->
			{noreply, State#state{playlist=Playlist}}
	end;

handle_cast(Msg, State) ->
	%% wrong message
	lager:warning("!!! unexpected cast received !!!"),
	lager:warning("~p", [Msg]),
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
	lager:warning("!!! unexpected info received !!!"),
	lager:warning("~p", [Info]),
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

