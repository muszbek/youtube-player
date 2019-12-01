from django.shortcuts import render
from django.http import HttpResponse, HttpResponseRedirect
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
        print("lol callback received")
        
        payload = {"url":"https://www.youtube.com/watch?v=nNPnQJUuAyc", "id":"tamas_django"}
        response = requests.post(SERVER_URL, json=payload)
        
        print(response.text)
        print(response.status_code, response.reason)
        
    return HttpResponseRedirect("../")
    