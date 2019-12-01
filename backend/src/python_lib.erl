%% @author tmuszbek
%% @doc @todo Add description to python_lib.


-module(python_lib).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_python/1, play_video/2, stop_player/1]).

start_python(ServerID) ->
	{ok, Path} = application:get_env(youtube_player, python_path),
	Options = [{python_path, Path}, {python, "python3"}],
	{ok, Pid} = python:start(Options),
	register_handler(Pid, ServerID),
	Pid.

play_video(Pid, Url) ->
	python:call(Pid, 'youtubePlayer.player', play_video, [Url]).

stop_player(Pid) ->
	python:call(Pid, 'youtubePlayer.player', stop, []),
	python:stop(Pid).


%% ====================================================================
%% Internal functions
%% ====================================================================

register_handler(PythonID, ServerID) ->
	python:call(PythonID, 'youtubePlayer.player', register_handler, [ServerID]).
