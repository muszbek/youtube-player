## System image
FROM python:3.7.4-slim-stretch as system
LABEL develop=true
		
RUN		pip3 install requests Django

## Build image
FROM system as builder
		
COPY	. /root/youtube-player/frontend

ENV		PYTHONPATH=/root/youtube-player/frontend

WORKDIR /root/youtube-player/frontend

EXPOSE  8000

CMD		python3 manage.py runserver 0.0.0.0:8000