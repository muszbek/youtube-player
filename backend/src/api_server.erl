%% @author tmuszbek
%% @doc @todo Add description to api_server.


-module(api_server).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).

-include("playlist_server.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
%% record to map macro, cannot define as function because of record_info
-define(RtoM(Name, Record), lists:foldl(fun({I, E}, Acc) -> Acc#{E => element(I, Record)} end, 
										#{}, 
										lists:zip(lists:seq(2, (record_info(size, Name))), 
												  (record_info(fields, Name))))).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {}).

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
	lager:debug("API server started"),
	start_endpoints(),
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
handle_call(_Request, _From, State) ->
    Reply = ok,
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
handle_cast(_Msg, State) ->
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
handle_info(_Info, State) ->
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

start_endpoints() ->
	rooster:start(#{port => get_port()},
				  #{routes => exports(),
					middleware => []}).

exports() ->
	[{"/playlist", [],
	  [{'GET', "/hello", fun hello/1},
	   {'POST', fun post_video/1},
	   {'GET', "/list", fun get_playlist/1}]
	 }].

hello(_Request) ->
	{200, #{message => <<"playlist_server_present">>}}.

post_video(#{body := Body}) ->
	lager:debug("Video posted on REST API: ~p", [Body]),
	Id = maps:get(id, Body),
	Video = maps:get(url, Body),
	_IsOk = playlist_server:publish_video(Video, Id),
	{201, Body}.

get_playlist(_Request) ->
	{CurrVid, Playlist} = playlist_server:get_playlist(),
	MapCurrVid = ?RtoM(video, CurrVid),
	MapPlaylist = lists:map(fun(VidRecord) -> ?RtoM(video, VidRecord) end, Playlist),
	JsonReply = jsx:encode(#{current_video => MapCurrVid,
							 playlist => MapPlaylist}),
	lager:debug("Playlist reply: ~p", [JsonReply]),
	{200, #{message => JsonReply}}.

get_port() ->
	case os:getenv("YP_BACKEND_PORT") of
		false ->
			{ok, Port} = application:get_env(youtube_player, rest_port),
			Port;
		Port -> Port
	end.

%% test:
%% curl -d '{"url":"https://www.youtube.com/watch?v=nNPnQJUuAyc", "id":"muszitest"}' -X POST http://localhost:8081/playlist
