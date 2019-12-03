from django.shortcuts import render
from django.http import HttpResponse, HttpResponseRedirect
from django.core.validators import URLValidator
from django.core.exceptions import ValidationError
import requests

# Create your views here.
HTML_PATH = "./frontend/views/main.html"

SERVER_URL = "http://192.168.0.12:8081/playlist"

def index(request):
    with open(HTML_PATH, 'r') as htmlFile:
        htmlText = htmlFile.read()
    
    return HttpResponse(htmlText)

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
            
            print(response.text)
            print(response.status_code, response.reason)
            
        except ValidationError as e:
            print(e)
        
    return HttpResponseRedirect("../")
    