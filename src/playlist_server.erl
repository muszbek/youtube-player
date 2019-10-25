%% @author tmuszbek
%% @doc @todo Add description to playlist_server.


-module(playlist_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, next_video/0]).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

next_video() ->
	gen_server:cast(?SERVER, next_video).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {playlist=[],
				current_video}).

-record(video, {url="",
				publisher}).

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
handle_call({publish_video, Url}, From, State=#state{playlist=Playlist}) ->
    lager:debug("Video published to playlist: ~p", [Url]),
	Video = #video{url=Url, publisher=From},
	NewPlaylist = Playlist ++ [Video],	%% new video goes to back of playlist

	next_video(),
	Reply = ok,
    {reply, Reply, State#state{playlist=NewPlaylist}};

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
    {noreply, State};

handle_cast(next_video, State=#state{playlist=Playlist}) ->
    lager:debug("Call for next video"),
	[NextVideo | RemainingPlaylist] = Playlist,
	{Url, _Publisher} = NextVideo,
	
	case youtube_player_fsm:new_video(Url) of
		ok ->
			{noreply, State=#state{playlist=RemainingPlaylist, current_video=NextVideo}};
		busy ->
			{noreply, State=#state{playlist=Playlist}}
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

