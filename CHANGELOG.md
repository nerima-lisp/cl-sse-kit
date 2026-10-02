# Changelog

## 1.1.0

Follow cl-http-kit 0.4.0 for client transport while retaining
cl-http-message-kit for SSE request and response models. Add a loopback HTTPS
SSE test covering reconnect and Last-Event-ID.

## 1.0.0

First stable release.

- Server-Sent Events wire parser and serializer, one-shot and incremental,
  following the WHATWG Server-sent events specification.
- HTTP response construction and request validation for SSE endpoints,
  built on `cl-http-message-kit`.
- Bounded server sessions with heartbeats, output budgets, and idempotent
  close.
- A broadcast publisher with bounded replay history and per-session
  delivery queues.
- Transport-neutral client protocol state with reconnection, backoff, and
  `Retry-After` handling via `cl-resilience-kit`.
- Continuation-style (`/k`) entry points across the parser, session, and
  client APIs.
