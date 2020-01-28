from django.urls import path

from . import views

urlpatterns = [
    path('', views.index, name='index'),
    path('send_url/', views.send_url),
    path('server_refresh/', views.server_refresh)
]