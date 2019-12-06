import json
import os

currentFolder = os.path.dirname(__file__)
CONFIG_PATH = currentFolder + '/config.json'

with open(CONFIG_PATH) as jsonFile:
    jsonConfig = json.load(jsonFile)
    
try:
    BACKEND_ADDRESS = os.environ["YP_BACKEND_ADDRESS"]
except KeyError:
    BACKEND_ADDRESS = jsonConfig["backend_address"]
    
try:
    BACKEND_PORT = os.environ["YP_BACKEND_PORT"]
except KeyError:
    BACKEND_PORT = jsonConfig["backend_port"]
    
try:
    FRONTEND_ADDRESS = os.environ["YP_FRONTEND_ADDRESS"]
except KeyError:
    FRONTEND_ADDRESS = jsonConfig["frontend_address"]