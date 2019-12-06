## System image
FROM ubuntu:18.04 as system
LABEL develop=true

RUN		apt-get update && apt-get -y install \
			erlang \
			wget \
			build-essential \
			erlang-dev \
			git \
			python3 \
			python3-pip \
			vlc

RUN		wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3 && \
		./rebar3 local install
		
RUN		pip3 install --user pafy youtube-dl python-vlc
		
		
## Build image
FROM system as builder

COPY	. /root/youtube-player/backend
					
RUN 	cd ~/youtube-player/backend && \
		rm -rdf _build/* && \
		~/.cache/rebar3/bin/rebar3 release
		
ENV		YP_PYTHONPATH=/root/youtube-player/backend/python/src

WORKDIR /root/youtube-player/backend

CMD		./_build/default/rel/youtube_player/bin/youtube_player console
