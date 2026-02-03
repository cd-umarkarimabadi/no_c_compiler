import socket
client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

try:
    client_socket.connect(('localhost', 8080))
    print("Connected to localhost:8080")
    
    message = "hello"
    client_socket.sendall(message.encode('utf-8'))
    print(f"Sent: {message}")
    
    response = client_socket.recv(1024)
    print(f"Received: {response.decode('utf-8')}")
    
except ConnectionRefusedError:
    print("Connection refused. Make sure the server is running on port 8080.")
except Exception as e:
    print(f"An error occurred: {e}")
finally:
    # Close the connection
    client_socket.close()
    print("Connection closed")