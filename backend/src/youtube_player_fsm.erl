%% @author tmuszbek
%% @doc @todo Add description to youtube_player_fsm.


-module(youtube_player_fsm).
-behaviour(gen_fsm).
-compile([{parse_transform, lager_transform}]).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([down/2, down/3,
		 idle/2, idle/3,
		 playing/2, playing/3]).

-define(SERVER, ?MODULE).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, new_video/1]).

start_link() ->
	gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

new_video(Url) ->
	case is_server_alive() of
		true ->
			gen_fsm:sync_send_event(?SERVER, {new_video, Url});
		false ->
			video_refused
	end.


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {current_video,
				python_server_monitor}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:init-1">gen_fsm:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, StateName, StateData}
			| {ok, StateName, StateData, Timeout}
			| {ok, StateName, StateData, hibernate}
			| {stop, Reason}
			| ignore,
	StateName :: atom(),
	StateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
init([]) ->
	lager:debug("FSM started"),
	Ref = monitor(process, python_server),
	startup_get_video(),
	{ok, idle, #state{python_server_monitor=Ref}}.


%% down/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-2">gen_fsm:StateName/2</a>
-spec down(Event :: timeout | term(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
% @todo implement actual state
down(python_up, StateData) ->
	Ref = monitor(process, python_server),
	startup_get_video(),
    {next_state, idle, StateData#state{python_server_monitor=Ref}};

down(Event, StateData) ->
	unexpected(event, Event, down),
    {next_state, down, StateData}.

%% down/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-3">gen_fsm:StateName/3</a>
-spec down(Event :: term(), From :: {pid(), Tag :: term()}, StateData :: term()) -> Result when
	Result :: {reply, Reply, NextStateName, NewStateData}
			| {reply, Reply, NextStateName, NewStateData, Timeout}
			| {reply, Reply, NextStateName, NewStateData, hibernate}
			| {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, Reply, NewStateData}
			| {stop, Reason, NewStateData},
	Reply :: term(),
	NextStateName :: atom(),
	NewStateData :: atom(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: normal | term().
%% ====================================================================
down({new_video, _Url}, _From, StateData) ->
	lager:debug("new_video event to fsm while down, doing nothing"),
	Reply = video_refused,
    {reply, Reply, down, StateData};

down(Event, _From, StateData) ->
	unexpected(sync_event, Event, down),
    Reply = {error, {unexpected_sync_event, down, Event}},
    {reply, Reply, down, StateData}.


%% idle/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-2">gen_fsm:StateName/2</a>
-spec idle(Event :: timeout | term(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
% @todo implement actual state
idle(Event, StateData) ->
	unexpected(event, Event, idle),
    {next_state, idle, StateData}.

%% idle/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-3">gen_fsm:StateName/3</a>
-spec idle(Event :: term(), From :: {pid(), Tag :: term()}, StateData :: term()) -> Result when
	Result :: {reply, Reply, NextStateName, NewStateData}
			| {reply, Reply, NextStateName, NewStateData, Timeout}
			| {reply, Reply, NextStateName, NewStateData, hibernate}
			| {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, Reply, NewStateData}
			| {stop, Reason, NewStateData},
	Reply :: term(),
	NextStateName :: atom(),
	NewStateData :: atom(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: normal | term().
%% ====================================================================
idle({new_video, Url}, _From, StateData) ->
	%% called by playlist_server
	%% this event is responsible for starting or rejecting a new video
	%% it does not change state
	lager:debug("new_video event to fsm while idle, playing video ~p", [Url]),
	python_server:play_video(Url),
	Reply = video_accepted,
	{reply, Reply, idle, StateData};

idle({starting_video, Url}, _From, StateData) ->
	%% called by python_server
	%% this event is responsible for changing state
	Reply = ok,
	{reply, Reply, playing, StateData#state{current_video=Url}};

idle(Event, _From, StateData) ->
	unexpected(sync_event, Event, idle),
    Reply = {error, {unexpected_sync_event, idle, Event}},
    {reply, Reply, idle, StateData}.


%% playing/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-2">gen_fsm:StateName/2</a>
-spec playing(Event :: timeout | term(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
% @todo implement actual state
playing(Event, StateData) ->
	unexpected(event, Event, playing),
    {next_state, playing, StateData}.

%% playing/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-3">gen_fsm:StateName/3</a>
-spec playing(Event :: term(), From :: {pid(), Tag :: term()}, StateData :: term()) -> Result when
	Result :: {reply, Reply, NextStateName, NewStateData}
			| {reply, Reply, NextStateName, NewStateData, Timeout}
			| {reply, Reply, NextStateName, NewStateData, hibernate}
			| {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, Reply, NewStateData}
			| {stop, Reason, NewStateData},
	Reply :: term(),
	NextStateName :: atom(),
	NewStateData :: atom(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: normal | term().
%% ====================================================================
playing({new_video, _Url}, _From, StateData) ->
	lager:debug("new_video event to fsm while playing, doing nothing"),
	Reply = video_refused,
    {reply, Reply, playing, StateData};

playing({finishing_video, Url}, _From, StateData=#state{current_video=Url}) ->
	playlist_server:next_video(),
	Reply = ok,
    {reply, Reply, idle, StateData#state{current_video= << >>}};

playing({starting_video, Url}, _From, StateData) ->
	%% TODO: more explicit implementation of interrupting videos
	lager:warning("Unexpected play video request, current one is still not finished!", []),
	Reply = ok,
	{reply, Reply, playing, StateData#state{current_video=Url}};

playing(Event, _From, StateData) ->
	unexpected(sync_event, Event, playing),
    Reply = {error, {unexpected_sync_event, playing, Event}},
    {reply, Reply, playing, StateData}.


%% handle_event/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_event-3">gen_fsm:handle_event/3</a>
-spec handle_event(Event :: term(), StateName :: atom(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_event(Event, StateName, StateData) ->
	unexpected(event, Event, StateName),
    {next_state, StateName, StateData}.


%% handle_sync_event/4
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_sync_event-4">gen_fsm:handle_sync_event/4</a>
-spec handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()}, StateName :: atom(), StateData :: term()) -> Result when
	Result :: {reply, Reply, NextStateName, NewStateData}
			| {reply, Reply, NextStateName, NewStateData, Timeout}
			| {reply, Reply, NextStateName, NewStateData, hibernate}
			| {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, Reply, NewStateData}
			| {stop, Reason, NewStateData},
	Reply :: term(),
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_sync_event(stop, _From, _StateName, StateData) ->
	lager:debug("Stopping youtube_player_fsm normally"),
    {stop, normal, shutdown_ok, StateData};

handle_sync_event(Event, _From, StateName, StateData) ->
	unexpected(sync_event, Event, StateName),
    Reply = ok,
    {reply, Reply, StateName, StateData}.


%% handle_info/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_info-3">gen_fsm:handle_info/3</a>
-spec handle_info(Info :: term(), StateName :: atom(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: normal | term().
%% ====================================================================
handle_info({'DOWN', _Ref, process, {python_server, _Node}, noproc}, _StateName, StateData) ->
	lager:warning("init monitor did not find python_server"),
	{next_state, down, StateData};

handle_info({'DOWN', _Ref, process, {python_server, _Node}, _Reason}, _StateName, StateData) ->
	lager:warning("python_server down"),
	{next_state, down, StateData};

handle_info(Info, StateName, StateData) ->
	unexpected(info, Info, StateName),
    {next_state, StateName, StateData}.


%% terminate/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:terminate-3">gen_fsm:terminate/3</a>
-spec terminate(Reason, StateName :: atom(), StateData :: term()) -> Result :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _StateName, _StatData) ->
    ok.


%% code_change/4
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:code_change-4">gen_fsm:code_change/4</a>
-spec code_change(OldVsn, StateName :: atom(), StateData :: term(), Extra :: term()) -> {ok, NextStateName :: atom(), NewStateData :: term()} when
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%% ====================================================================
%% Internal functions
%% ====================================================================

is_server_alive() ->
	is_server_alive(?SERVER).

is_server_alive(ServerName) ->
	case whereis(ServerName) of
		undefined ->
			false;
		_Pid ->
			is_process_alive(whereis(ServerName))
	end.

startup_get_video() ->
	case is_server_alive(playlist_server) of
		true ->
			playlist_server:replay_video();
		false ->
			ok	%% no playlist_server, no video to play
	end.
	
unexpected(Type, Msg, State) ->
	lager:warning("~p received unknown ~p ~p while in state ~p~n",
			  [self(), Type, Msg, State]).
