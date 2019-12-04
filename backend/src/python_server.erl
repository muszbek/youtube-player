%% @author tmuszbek
%% @doc @todo Add description to python_server.


-module(python_server).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define(SERVER, ?MODULE).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, play_video/1]).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

play_video(Url) ->
	gen_server:cast(?SERVER, {play, Url}).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {python_id,
				last_played=""}).

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
	lager:debug("Python server started"),
	Python = python_lib:start_python(whereis(?SERVER)),
	link(Python),	%%if python dies, the server needs to die with it, supervisor restarts both
	case whereis(youtube_player_fsm) == undefined of
		true ->
			%% Do nothing, cannot send event to fsm if it does not exist yet
			ok;
		false ->
			gen_fsm:send_event(youtube_player_fsm, python_up)
	end,
	%% TODO: maybe change this inline call to a gen_server:call
    {ok, #state{python_id=Python}}.


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
handle_call(stop, _From, State) ->
	lager:debug("Stopping python_server normally"),
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
handle_cast({play, Url}, State=#state{python_id=Python}) ->
	lager:debug("Playing video ~p", [Url]),
	case python_lib:play_video(Python, Url) of
		ok ->
			gen_fsm:sync_send_event(youtube_player_fsm, {starting_video, Url}),
			{noreply, State#state{last_played=Url}};
		wrong_url_error ->
			gen_fsm:sync_send_event(youtube_player_fsm, {error_skipping_video, Url}),
			{noreply, State}
	end;

handle_cast(finished, State=#state{last_played=Url}) ->
	lager:info("got cast finished"),
	gen_fsm:sync_send_event(youtube_player_fsm, {finishing_video, Url}),
    {noreply, State};

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
terminate(_Reason, _State=#state{python_id=Python}) ->
	python_lib:stop_player(Python),
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


