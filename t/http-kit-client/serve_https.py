import json
import socket
import ssl
import sys
import time


mode, certificate, key = sys.argv[1:]
responses = {
    "reconnect": [
        (200, "text/event-stream", b"event: update\ndata: first\nid: first-id\nretry: 250\n\n", []),
        (200, "text/event-stream", b"event: finish\ndata: second\nid: second-id\n\n", []),
    ],
    "stop": [
        (200, "text/event-stream", b"data: once\nid: stop-id\nretry: 1000\n\n", []),
    ],
    "partial-id": [
        (200, "text/event-stream", b"id: uncommitted\n", []),
        (200, "text/event-stream", b"data: recovered\nid: committed\n\n", []),
    ],
    "retry-limit": [
        (500, "text/plain", b"server error", []),
        (500, "text/plain", b"server error", []),
        (500, "text/plain", b"server error", []),
    ],
    "callback-error": [
        (200, "text/event-stream", b"data: callback\nid: callback-id\n\n", []),
    ],
}

context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
context.load_cert_chain(certificate, key)
listener = socket.socket()
listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
listener.bind(("127.0.0.1", 0))
listener.listen()
print(listener.getsockname()[1], flush=True)

try:
    if mode == "redirect":
        request_count = 2
    else:
        request_count = len(responses[mode])

    for index in range(request_count):
        connection, _ = listener.accept()
        with connection, context.wrap_socket(connection, server_side=True) as stream:
            request = bytearray()
            while b"\r\n\r\n" not in request:
                chunk = stream.recv(4096)
                if not chunk:
                    raise RuntimeError("client closed before sending request headers")
                request.extend(chunk)

            lines = bytes(request).split(b"\r\n")
            method, path, _ = lines[0].decode("ascii").split(" ", 2)
            headers = {}
            for line in lines[1:]:
                if not line:
                    break
                name, value = line.decode("latin-1").split(":", 1)
                headers[name.lower()] = value.strip()
            print(json.dumps({
                "method": method,
                "path": path,
                "headers": headers,
                "at": time.monotonic(),
            }, separators=(",", ":")), flush=True)

            if mode == "redirect" and index == 0:
                status = 302
                content_type = None
                body = b""
                extra_headers = [
                    ("Location", "/events"),
                    ("Set-Cookie", "sid=loopback; Path=/; Secure"),
                ]
            elif mode == "redirect":
                status = 200
                content_type = "text/event-stream"
                body = b"event: redirected\ndata: followed\nid: redirect-id\n\n"
                extra_headers = []
            else:
                status, content_type, body, extra_headers = responses[mode][index]

            reason = "Found" if status == 302 else "OK"
            response = [f"HTTP/1.1 {status} {reason}\r\n"]
            if content_type:
                response.append(f"Content-Type: {content_type}\r\n")
            response.extend(f"{name}: {value}\r\n" for name, value in extra_headers)
            response.extend([
                f"Content-Length: {len(body)}\r\n",
                "Connection: close\r\n",
                "\r\n",
            ])
            stream.sendall("".join(response).encode("ascii") + body)
finally:
    listener.close()
