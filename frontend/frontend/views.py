from django.shortcuts import render
from django.http import HttpResponse, HttpResponseRedirect
from django.core.validators import URLValidator
from django.core.exceptions import ValidationError
from django.template import loader
from urllib3.connection import NewConnectionError
from urllib3.exceptions import MaxRetryError
from requests.exceptions import ConnectionError
import requests
import json
import configReader

# Create your views here.
HTML_PATH = "./frontend/views/main.html"

SERVER_ADDRESS = configReader.BACKEND_ADDRESS + ":" + str(configReader.BACKEND_PORT)
SERVER_URL = "http://" + SERVER_ADDRESS + "/playlist"


def index(request):
    serverAddress = _getServerAddressText()
    
    serverContext = {
        'backend_address': serverAddress,
    }
    
    playlistContext = _createPlaylistContext(_isServerReachable(serverAddress))
    
    context = {**serverContext, **playlistContext}
    template = loader.get_template('frontend/main.html')
    
    return HttpResponse(template.render(context, request))


def server_refresh(request):
    return HttpResponseRedirect("../")
        

def send_url(request):
    if(request.GET.get('send_url_button')):
        
        sentUrl = request.GET.get('url_input_field')
        senderID = request.GET.get('self_id')
        print(sentUrl)
        print(senderID)
        
        val = URLValidator()
        try:
            val(sentUrl)
            
            payload = {'url': sentUrl, 'publisher': senderID}
            response = requests.post(SERVER_URL, json=payload)
            print("*** Video sent to playlist ***")
            
        except ValidationError as e:
            print(e)
            
        except (ConnectionRefusedError, NewConnectionError, MaxRetryError, 
                ConnectionError, ConnectionResetError) as e:
            pass    # redirect will expose that the server is down
        
    return HttpResponseRedirect("../")


def playlist_remove(request):
    if(request.GET.get('remove_button')):
        senderID = request.GET.get('self_id_playlist')
        selectedID = request.GET.get('title_list')
        
        if selectedID and selectedID != "undefined":
            print("Removing from playlist: " + selectedID)
            
            payload = {'id': int(selectedID), 'publisher': senderID}
            response = requests.post(SERVER_URL + "/remove", json=payload)
        
    return HttpResponseRedirect("../")


def _getServerAddressText():
    try:
        response = requests.get(SERVER_URL + "/hello")
        if json.loads(response.text)['message'] == "playlist_server_present":
            return SERVER_ADDRESS
        else:
            return "DOWN"
        
    except (ConnectionRefusedError, NewConnectionError, MaxRetryError, 
            ConnectionError, ConnectionResetError) as e:
        print("!!! Back-end server DOWN !!!")
        return "DOWN"

def _isServerReachable(serverAddress):
    return serverAddress != "DOWN"

def _createPlaylistContext(isServerReachable):
    if isServerReachable:
        return _getPlaylistContext()
    else:
        return {'playlist': []}

def _getPlaylistContext():
    response = requests.get(SERVER_URL + "/list")
    body = json.loads(response.text)['message']
    
    return {
        'current_video': body['current_video'],
        'playlist': body['playlist']
    }
    