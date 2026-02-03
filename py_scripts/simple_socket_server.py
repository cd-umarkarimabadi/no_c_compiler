import socket
import time


s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.bind(('127.0.0.1', 8080))
s.listen(1)
print('Server running on 127.0.0.1:8080')

while True:
    conn, addr = s.accept()
    conn.recv(1024)
    print("recv'd data will go to sleep now")
    time.sleep(30) 
    conn.sendall(b'Hello client\n')
    conn.close()