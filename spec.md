Requirements:
- Real time youtube player from playlist
- Playlist is FIFO queue
- Clients on local network can real time connect and disconnect to the socket server
- Clients can real time send youtube url-s to playlist
- Youtube player is in waiting while playlist is empty

Elements:
- Youtube player fsm (down, idle, playing)
- Youtube player python impl
- playlist server - stores the queue, serves the player fsm
- REST server - accepts requests, forwards video requests to playlist server
- REST front-end (django? android app?)

FSM:
initial state down
python_server is monitored
enter down by global trapped exit of python_server
async ask python_server if up, he async responds or sends message when up
-> transit to idle

idle
async ask playlist for song
playlist sync pushes song when it is not empty, or when asked by FSM
-> transit to playing

playing
async receives finished event
-> transit to idle