# cl-sse-kit

`cl-sse-kit` is a Common Lisp implementation of the HTTP Server-Sent Events
protocol. It provides the wire parser and serializer, incremental stream
state, HTTP metadata validation, bounded server sessions, replay history, and
transport-neutral client reconnection state.

The transport boundary is explicit: an HTTP server or client owns sockets,
request lifetimes, timers, backpressure, and partial writes. This library
operates directly on `cl-http-message-kit:http-response`,
`cl-http-message-kit:http-header`, octet vectors, and callbacks.

## Install

The project is tested with ASDF and Nix. The runtime dependencies are:

- `nerima-lisp/cl-codec-kit` 0.5.0 for UTF-8 conversion with replacement of malformed octets;
- `nerima-lisp/cl-http-message-kit` for HTTP request/response values;
- `nerima-lisp/cl-resilience-kit` 1.0.0 for retry policy and backoff;
- `nerima-lisp/cl-weave` 1.3.0 for tests and coverage.

```lisp
(asdf:load-system "cl-sse-kit")
```

## Standards model

The parser follows the [WHATWG Server-sent events
specification](https://html.spec.whatwg.org/multipage/server-sent-events.html):

- input is UTF-8 and accepts LF, CRLF, or bare CR line endings;
- an optional UTF-8 BOM is ignored at the beginning of the stream;
- one optional space after a field colon is removed;
- `data` lines are joined with LF and dispatch only happens at a blank line;
- `id` persists as the `Last-Event-ID` cursor and ignores values containing NUL;
- `retry` persists as the reconnection delay and accepts only ASCII digits;
- strict EOF discards an unterminated event.

There is one strict parsing contract. Both `parse-http-sse-events` and
`finish-http-sse-parser` discard an event that has not ended with a blank line.

## Parsing and serialization

```lisp
(let ((parser
        (sse-kit:make-http-sse-parser
         :on-event (lambda (event)
                     (format t "~A: ~A~%"
                             (sse-kit:http-sse-event-event event)
                             (sse-kit:http-sse-event-data event))))))
  (sse-kit:feed-http-sse-parser parser body-chunk)
  (sse-kit:finish-http-sse-parser parser))
```

`feed-http-sse-parser` accepts a string or an unsigned-byte octet vector, with
optional `:start` and `:end` bounds. Chunks may split a UTF-8 code point or a
CRLF pair. The parser exposes the persistent cursor, retry delay, event count,
collected events, and finished state.

For one-shot parsing and serialization:

```lisp
(sse-kit:parse-http-sse-events
 "event: greet\ndata: hello\ndata: world\nid: 7\n\n")

(sse-kit:serialize-http-sse-event
 (sse-kit:make-http-sse-event :event "greet" :data "hello" :id "7"))
```

Serialized events are UTF-8 octet vectors using CRLF wire endings. Use
`read-http-sse-events` when the host owns a character or binary input stream;
the function buffers reads, feeds the incremental parser, and finishes it
strictly. Both it and `parse-http-sse-events` accept
`:initial-last-event-id`, which seeds the replay cursor without emitting an
event. Use `write-http-sse-event` for one complete event sent to a binary stream
or transport callback. `:max-bytes` can bound serialized output.

The `/k` entry points and `with-http-sse-parser` macro expose continuation-style
success and error paths.

## HTTP response and server sessions

```lisp
(let* ((response
         (sse-kit:make-http-sse-response
          :x-accel-buffering "no"))
       (session
         (sse-kit:make-http-sse-session
          :response response
          :write-octets #'write-one-octet-vector
          :max-output-bytes (* 8 1024 1024))))
  (sse-kit:send-http-sse-event
   session
   (sse-kit:make-http-sse-event :data "ready" :id "1"))
  (sse-kit:send-http-sse-comment session "heartbeat")
  (sse-kit:flush-http-sse-session session))
```

`make-http-sse-response` supplies the SSE content type and no-cache policy and
manages optional CORS and proxy-buffering headers. An explicit non-wildcard
`:cors-origin` also adds `Vary: Origin`; credentials require such an origin.
The caller must derive `:cors-origin` from its own origin allowlist; the helper
does not authorize the request's `Origin` header.
`validate-http-sse-request` checks the GET method, `Accept`, request body, and
`Last-Event-ID`, signaling `sse-http-error` with an HTTP status when a request
cannot be served. `Last-Event-ID` and response `Content-Type` are treated as
single-valued headers; duplicate values are rejected at the protocol boundary.
`http-sse-request-last-event-id` returns the validated request cursor as a
Unicode string, or the empty string when no cursor was sent. HTTP Message Kit
represents header contents as octet-valued strings, so the client's generated
`Last-Event-ID` header contains the cursor's UTF-8 bytes at that adapter
boundary.

For adapters that stream the HTTP body directly, use
`make-http-sse-response-stream`. Its `body-function` returns one-dimensional
non-empty octet vectors until it returns `NIL`, and `body-length` can provide
the exact representation length when the adapter needs a non-chunked response.

`make-http-sse-session` requires a direct `http-message-kit:http-response`
with status 200 and an SSE content type. The session tracks bytes and event
count, supports flush and close callbacks, is idempotently closeable, and
enforces an optional output budget before writing. Use
`http-sse-session-heartbeat-due-p` with the configured heartbeat interval and
`send-http-sse-heartbeat` to send a comment heartbeat and update its deadline.
When `:stream` is used instead of `:write-octets`, it must be an octet/binary
output stream; use `:write-octets` for adapters that own transport writes.

The writer callback receives one complete octet vector per operation. An HTTP
adapter can apply its own socket backpressure and deadlines around that call.
For a session shared by multiple producer or heartbeat tasks, pass `:synchronize`;
it receives a thunk and must execute it under the host application's session lock.
Event, comment, heartbeat, flush, and close operations use that boundary. The
`:write-octets` callback runs inside that same boundary, so code that closes,
unsubscribes, or republishes to this session from within its own write
callback will re-enter the lock on the same call stack; use a reentrant lock
for `:synchronize`, or avoid such inline calls, if this pattern applies.

## Publishing and replay

```lisp
(let ((publisher (sse-kit:make-http-sse-publisher :max-history 1000)))
  (sse-kit:subscribe-http-sse-session publisher session
                                      :last-event-id client-cursor)
  (sse-kit:publish-http-sse-event
   publisher
   (sse-kit:make-http-sse-event :id "2" :data "update")))
```

History is bounded and retained oldest-first. A non-empty replay cursor must
still be present in retained history; otherwise `sse-replay-unavailable` is
signaled instead of silently sending an incomplete stream. Failed session
writes are returned as conditions, removed from the publisher, and closed.
Publishes that occur while a session is replaying or delivering are queued and
drained in order, so a replay cannot miss a concurrently published event.
Applications that use threads should provide `:synchronize` to
`make-http-sse-publisher`; it receives a thunk and runs it under the
application's lock.
`MAX-QUEUE` defaults to 1000 and bounds events waiting behind replay or an
in-progress delivery. An overflow removes and closes the subscriber with
`sse-size-limit-exceeded` (`:publisher-queue`); use `:max-queue nil` only when
the host has a separate backpressure policy. The bound is available through
`http-sse-publisher-max-queue`.
Event IDs should be unique within retained history; when duplicates exist,
replay resumes after the newest matching ID. The configured bound is available
through `http-sse-publisher-max-history`.

## Client protocol state

```lisp
(let ((client (sse-kit:make-http-sse-client
               :retry-policy
               (resilience-kit:make-retry-policy
                :max-attempts 8
               :initial-delay 1d0
               :max-delay 30d0
               :retry-safe-p t)
               :initial-last-event-id persisted-cursor
               :on-event #'handle-event
               :on-error #'handle-error
               :on-close #'handle-close)))
  (sse-kit:http-sse-client-request-headers client)
  (sse-kit:start-http-sse-client-response
   client
   response)
  (sse-kit:feed-http-sse-client client body-chunk)
  (sse-kit:finish-http-sse-client-response client :eof))
```

`initial-last-event-id` seeds the cursor used in the first request, which is
useful when the application persists the cursor outside the client. The
response passed to `start-http-sse-client-response` may be a direct
`http-message-kit:http-response` or `http-response-stream`. The client
validates status and content type, sends `Last-Event-ID` after an event
establishes a cursor, applies server `retry` values and both delta-seconds and
HTTP-date `Retry-After` values, and
delegates retry classification and backoff to `cl-resilience-kit`. Reconnects
are persistent by default; pass `:retry-forever-p nil` to enforce the retry
policy's attempt bound. HTTP 204 permanently stops reconnection. Redirects
are surfaced through `:on-redirect`, while the host owns the actual request,
redirect transport, and timer.

`on-close` is called when a response ends, including a response that will be
retried; it receives the end reason while retrying and `:retry-exhausted` when
the client becomes terminal. `stop-http-sse-client` cancels the active
transport at most once and transitions the client to `CLOSED`.

`read-http-sse-client-response` requires `:element-type :octets` for a binary
octet stream and `:element-type :characters` for a character stream; mismatched
input is rejected before reading. Transport body read failures and invalid body chunks follow the same error,
retry, cancellation, and close path as parser failures. The client does not
open sockets or follow redirects itself; the host owns those transport
operations and supplies the next response.

Lifecycle notification callbacks (`:on-open`, `:on-error`, `:on-close`,
`:on-retry`, and `:on-redirect`) are isolated from protocol failures: a
condition signaled by one is retained as the most recent
`http-sse-client-callback-error` and cannot mask the transport error that
triggered the notification. `:on-event` remains part of the body-processing
operation, so its conditions propagate through `feed-http-sse-client` and the
corresponding read/consume operation.

## Transport adapter checklist

The library intentionally stops at the HTTP message and byte-stream boundary.
An adapter that connects it to a web server or HTTP client should:

1. Validate incoming metadata with `validate-http-sse-request` before creating a
   session.
2. Return `make-http-sse-response` (or
   `make-http-sse-response-stream`) and route body writes through a binary
   transport.
3. Register sessions with `make-http-sse-publisher` when replay or broadcast is
   required, and call `publish-http-sse-event` from the application producer.
4. Pass each received body chunk to `feed-http-sse-client` or use
   `read-http-sse-client-response`; schedule retry attempts from `:on-retry`.
5. Keep ownership of sockets, TLS, request lifetimes, timers, redirect
   requests, backpressure, partial writes, and application-level locks.

## Limits and errors

Parser, serializer, session, publisher, and client APIs expose bounded
operations. Parser limits include total input, line, data, comment, and event
budgets. `sse-size-limit-exceeded` provides `limit`, `observed`, and `kind`
slots. Protocol and HTTP failures use `sse-error` or its specialized
conditions, so adapters can distinguish malformed input, an unavailable
replay cursor, an invalid HTTP response, a stopped client, a disconnect, and
a resource limit without parsing error strings.

Field values containing line breaks are rejected before serialization. Header
names and values are validated by `cl-http-message-kit` before an adapter can
emit response-splitting input.

## Tests and coverage

Run the deterministic test system through the pinned Nix environment:

```shell
nix run .#test
```

The coverage command uses `cl-weave` with one worker, a five-minute process
timeout, and five-minute per-test timeouts. It emits a temporary HTML report
and fails unless expression and branch coverage reach 100%:

```shell
nix run .#coverage
```

## API overview

| Area | Entry points |
| --- | --- |
| Events | `make-http-sse-event`, event accessors |
| Parser | `make-http-sse-parser`, `feed-http-sse-parser`, `finish-http-sse-parser`, `parse-http-sse-events`, `read-http-sse-events` |
| Serializer | `serialize-http-sse-event`, `write-http-sse-event` |
| HTTP | `make-http-sse-response`, `make-http-sse-response-stream`, `validate-http-sse-request`, `http-sse-request-last-event-id`, `http-sse-content-type-valid-p` |
| Server | `make-http-sse-session`, send/comment/heartbeat/flush/close operations |
| Broadcast | `make-http-sse-publisher`, history/queue accessors, subscribe/unsubscribe/publish operations |
| Client | `make-http-sse-client`, request/response/feed/finish/stop operations |
| Conditions | `sse-error`, `sse-size-limit-exceeded`, `sse-http-error`, `sse-replay-unavailable`, client lifecycle conditions |

## License

MIT. See [LICENSE](LICENSE).
