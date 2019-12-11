youtube_player
=====

A web application to run a youtube playlist on a local network.

Users can submit their own youtube URL-s on the webpage, and the playlist plays on VLC player on the back-end machine.

Connect your phone to the same wifi as the back-end PC, then submit your songs on the website. No need to get up from your couch and do it on the PC.

*Unfinished*

Native Run
----
Back-end and front-end has to be run as two seperate processes, cd into their folders and see their respective README-s.

Docker Run
----

    $ xhost +
    $ docker-compose up
