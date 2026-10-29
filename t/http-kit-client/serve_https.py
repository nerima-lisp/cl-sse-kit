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
    "oversized-line": [
        (200, "text/event-stream", b"x" * 65537, []),
    ],
    "oversized-final-cr": [
        (200, "text/event-stream", (b"data: x\n\n" * 10000) + b"data: over\n\r", []),
    ],
    "id-only-event-id": [
        (200, "text/event-stream", b"id: 42\n\n", []),
        (200, "text/event-stream", b"data: finished\n\n", []),
    ],
    "empty-event-id": [
        (200, "text/event-stream", b"id:\n\n", []),
        (200, "text/event-stream", b"data: finished\n\n", []),
    ],
    "duplicate-content-type": [
        (200, "text/event-stream", b"data: rejected\n\n", [("Content-Type", "text/event-stream")]),
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
    "invalid-status": [
        (500, "text/event-stream", b"event: rejected\ndata: status\n\n", []),
    ],
    "non-200-status": [
        (202, "text/event-stream", b"event: rejected\ndata: status\n\n", []),
    ],
    "invalid-content-type": [
        (200, "text/plain", b"event: rejected\ndata: content-type\n\n", []),
    ],
    "redirect-invalid-final": [
        (302, None, b"", [("Location", "/events")]),
        (200, "text/plain", b"event: rejected\ndata: content-type\n\n", []),
    ],
    "valid-204": [
        (204, "text/event-stream", b"", []),
    ],
    "invalid-204-content-type": [
        (204, "text/plain", b"", []),
    ],
    "active-stop": [
        (200, "text/event-stream", b"data: active\n\n", []),
    ],
    "callback-error": [
        (200, "text/event-stream", b"data: callback\nid: callback-id\n\n", []),
    ],
}

redirect_statuses = {
    "redirect-300": 300,
    "redirect-301": 301,
    "redirect-302": 302,
    "redirect-303": 303,
    "redirect-307": 307,
    "redirect-308": 308,
}
redirect_modes = set(redirect_statuses) | {
    "redirect",
    "redirect-cross-origin",
    "redirect-cross-origin-terminal",
    "redirect-cross-origin-challenge",
    "redirect-downgrade",
    "redirect-downgrade-allowed",
    "redirect-hsts",
    "redirect-invalid-final",
    "redirect-limit",
    "redirect-configured-limit",
    "redirect-status-disabled",
}

context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
context.load_cert_chain(certificate, key)
listener = socket.socket()
listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
listener.bind(("127.0.0.1", 0))
listener.listen()
if mode in {
    "proxy-auth-redirect",
    "redirect-cross-origin-challenge",
    "redirect-300",
}:
    listener.settimeout(0.5)
print(listener.getsockname()[1], flush=True)

try:
    if mode == "proxy-auth-redirect":
        request_count = 2
    elif mode in redirect_modes:
        if mode == "redirect-limit":
            request_count = 11
        elif mode == "redirect-configured-limit":
            request_count = 2
        elif mode == "redirect-cross-origin-challenge":
            request_count = 3
        elif mode in {
            "redirect-cross-origin-terminal",
            "redirect-downgrade",
            "redirect-status-disabled",
        }:
            request_count = 1
        else:
            request_count = 2
    else:
        request_count = len(responses[mode])

    for index in range(request_count):
        try:
            connection, _ = listener.accept()
        except socket.timeout:
            break
        use_plain_stream = (
            mode == "proxy-auth-redirect"
            or (mode == "redirect-downgrade-allowed" and index == 1)
        )
        stream = (
            connection
            if use_plain_stream
            else context.wrap_socket(connection, server_side=True)
        )
        with stream:
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

            if mode == "retry-limit" and index < 2:
                continue

            if mode == "proxy-auth-redirect" and index == 0:
                status = 302
                content_type = None
                body = b""
                extra_headers = [
                    ("Location", "http://origin-two.invalid/events")
                ]
            elif mode == "proxy-auth-redirect":
                status = 200
                content_type = "text/event-stream"
                body = b"data: proxy redirected\n\n"
                extra_headers = []
            elif mode in {"redirect-limit", "redirect-configured-limit"}:
                status = 302
                content_type = None
                body = b""
                extra_headers = [("Location", "/redirect-limit")]
            elif mode == "redirect-status-disabled":
                status = 302
                content_type = "text/event-stream"
                body = b"event: rejected\ndata: status\n\n"
                extra_headers = [("Location", "/events")]
            elif mode == "redirect-cross-origin-challenge" and index == 1:
                status = 401
                content_type = None
                body = b""
                extra_headers = [("WWW-Authenticate", 'Basic realm="test"')]
            elif mode in redirect_modes and index == 0:
                status = redirect_statuses.get(mode, 302)
                content_type = None
                body = b""
                if mode in {
                    "redirect-cross-origin",
                    "redirect-cross-origin-terminal",
                    "redirect-cross-origin-challenge",
                }:
                    location = f"https://localhost:{listener.getsockname()[1]}/events"
                elif mode in {"redirect-downgrade", "redirect-downgrade-allowed"}:
                    host = "localhost" if mode == "redirect-downgrade-allowed" else "127.0.0.1"
                    location = f"http://{host}:{listener.getsockname()[1]}/events"
                elif mode == "redirect-hsts":
                    location = f"http://localhost:{listener.getsockname()[1]}/events"
                else:
                    location = "/events"
                extra_headers = [
                    ("Location", location),
                    ("Set-Cookie", "sid=loopback; Path=/; Secure"),
                ]
                if mode == "redirect-downgrade-allowed":
                    extra_headers.extend([
                        ("Set-Cookie", "lax=scheme; Path=/; SameSite=Lax"),
                        ("Set-Cookie", "strict=scheme; Path=/; SameSite=Strict"),
                    ])
            elif mode in redirect_modes:
                status = 200
                content_type = (
                    "text/plain" if mode == "redirect-invalid-final" else "text/event-stream"
                )
                body = (
                    b"event: rejected\ndata: content-type\n\n"
                    if mode == "redirect-invalid-final"
                    else b"event: redirected\ndata: followed\nid: redirect-id\n\n"
                )
                extra_headers = []
            else:
                status, content_type, body, extra_headers = responses[mode][index]

            reason = {
                300: "Multiple Choices",
                401: "Unauthorized",
                204: "No Content",
                301: "Moved Permanently",
                302: "Found",
                303: "See Other",
                307: "Temporary Redirect",
                308: "Permanent Redirect",
            }.get(status, "OK")
            response = [f"HTTP/1.1 {status} {reason}\r\n"]
            if content_type:
                response.append(f"Content-Type: {content_type}\r\n")
            response.extend(f"{name}: {value}\r\n" for name, value in extra_headers)
            if mode == "active-stop":
                response.extend([
                    "Transfer-Encoding: chunked\r\n",
                    "Connection: close\r\n",
                    "\r\n",
                ])
                chunk = f"{len(body):X}\r\n".encode("ascii") + body + b"\r\n"
                stream.sendall("".join(response).encode("ascii") + chunk)
                stream.settimeout(3)
                try:
                    while stream.recv(4096):
                        pass
                except (ssl.SSLError, socket.timeout):
                    pass
            else:
                if status != 204:
                    response.append(f"Content-Length: {len(body)}\r\n")
                response.extend(["Connection: close\r\n", "\r\n"])
                stream.sendall("".join(response).encode("ascii") + body)
finally:
    listener.close()
