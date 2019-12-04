'''
Created on Apr 10, 2019

@author: tmuszbek
'''
import pafy
import vlc
import time
from erlport.erlterms import Atom
from erlport import erlang

print("*** Starting VLC YouTube Player ***")
vlcInst = vlc.Instance()
player = vlcInst.media_player_new()
events = player.event_manager()


def register_handler(dest):
    def handler(message):
        erlang.cast(dest, message)
        print(message)
        
    def finishedCallback(event):
        erlang.cast(dest, (Atom(b"$gen_cast"), Atom(b"finished")))
        # without the prefix '$gen_cast' the message is not recognized as cast by gen_server
        print("*** Python youtube player finished playing ***")
    
    erlang.set_message_handler(handler)
    events.event_attach(vlc.EventType.MediaPlayerEndReached, finishedCallback)
    
    return Atom(b"ok")


def play_video(urlBin=b"https://www.youtube.com/watch?v=nNPnQJUuAyc"):
    url = urlBin.decode("utf-8")
    
    try:
        media = _getMedia(url, vlcInst)
        
        player.set_media(media)
        player.play()
        return Atom(b"ok")
    
    except ValueError:
        print("!!! Unable to play received URL !!!")
        return Atom(b"wrong_url_error")
    
    
def _getMedia(url, vlcInst):
    video = pafy.new(url)
    print(video.title)
    best = video.getbest()
    streamUrl = best.url
    
    return vlcInst.media_new(streamUrl)


def stop():
    player.stop()
    
    return Atom(b"ok")


if __name__ == '__main__':
    play_video()
    
    while True:
        pass
    

    