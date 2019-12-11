from django.shortcuts import render
from django.http import HttpResponse, HttpResponseRedirect
from django.core.validators import URLValidator
from django.core.exceptions import ValidationError
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
    with open(HTML_PATH, 'r') as htmlFile:
        htmlText = htmlFile.read()
        
    try:
        response = requests.get(SERVER_URL + "/hello")
        if json.loads(response.text)["message"] == "playlist_server_present":
            serverAddress = SERVER_ADDRESS
        else:
            serverAddress = "DOWN"
        
    except (ConnectionRefusedError, NewConnectionError, MaxRetryError, 
            ConnectionError, ConnectionResetError) as e:
        print("!!! Back-end server DOWN !!!")
        serverAddress = "DOWN"
    
    htmlText = htmlText.replace("*unknown_ip*", serverAddress, 1)
    
    return HttpResponse(htmlText)


def server_reconnect(request):
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
            
            payload = {"url": sentUrl, "id": senderID}
            response = requests.post(SERVER_URL, json=payload)
            print("*** Video sent to playlist ***")
            
        except ValidationError as e:
            print(e)
            
        except (ConnectionRefusedError, NewConnectionError, MaxRetryError, 
                ConnectionError, ConnectionResetError) as e:
            pass    # redirect will expose that the server is down
        
    return HttpResponseRedirect("../")
    