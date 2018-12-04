import socket

file = open("ip.txt","w")
file.write(socket.gethostbyname(socket.gethostname()))
file.close()