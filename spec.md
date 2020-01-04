#Requirements:

##0.1:
- Real time youtube player from playlist
- Playlist is FIFO queue
- Clients on local network can real time connect and disconnect to the socket server
- Clients can real time send youtube url-s to playlist
- Youtube player is in waiting while playlist is empty

##0.2:
- playlist can be requested over REST api
- playlist is stored in frontend
- changes in playlist are pushed to frontend

- users can cancel their own videos from playlist
- if video is ongoing, video can be stopped

#Elements:
- Youtube player fsm (down, idle, playing)
- Youtube player python impl
- playlist server - stores the queue, serves the player fsm
- REST server - accepts requests, forwards video requests to playlist server
- REST front-end (django? android app?)

##FSM:
initial state down
python_server is monitored
enter down by global trapped exit of python_server
async ask python_server if up, he async responds or sends message when up
-> transit to idle

###down
refuses messages cleanly, waiting for startup of python server

###idle
async ask playlist for song
playlist sync pushes song when it is not empty, or when asked by FSM
-> transit to playing

###playing
async receives finished event
-> transit to idle

