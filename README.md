# cl-sse-kit

[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

cl-sse-kit parses and serializes Server-Sent Events — the `text/event-stream`
format. It has no dependencies and performs no I/O: you hand it a body, or a
stream to read from, and it hands back event values.

## Install

```lisp
(asdf:load-system "cl-sse-kit")
```

## Usage

```lisp
(sse-kit:parse-http-sse-events
 (format nil "event: greet~%data: hello~%data: world~%id: 7~%~%"))
;; => one HTTP-SSE-EVENT with event "greet", id "7",
;;    and data "hello<newline>world"
```

Serialization returns UTF-8 octets, ready to write to a binary body:

```lisp
(sse-kit:serialize-http-sse-event
 (sse-kit:make-http-sse-event :data (format nil "a~%b")))
;; => octets for "data:a<CRLF>data:b<CRLF><CRLF>"
```

## API

| Operation | Purpose |
| --- | --- |
| `parse-http-sse-events` | Parse a string or octet vector into event values |
| `read-http-sse-events` | Read events from a stream |
| `serialize-http-sse-event` | Encode one event as UTF-8 octets |
| `make-http-sse-event` | Construct an event, validating its fields |
| `http-sse-event-event` / `-data` / `-id` / `-retry` / `-comments` | Accessors |
| `sse-error` | Malformed input, with `message`, `operation` and `detail` readers |
| `sse-size-limit-exceeded` | A budget was exceeded; carries `limit`, `observed` and `kind` |

## Behaviour worth knowing

Successive `data:` lines belong to one event and are joined with a line feed,
never with the line ending that separated them on the wire. CRLF, LF and a
bare CR are all accepted as separators.

Exactly one space after the field colon is optional padding and is stripped; a
second space is part of the value.

A leading colon marks a comment. Comments are kept on the event rather than
discarded, since they are how servers send keep-alives, but they never appear
in the data.

A `retry:` value that is not a base-ten integer is ignored rather than fatal,
so one malformed field cannot end a stream.

Field values containing a line break are refused at construction rather than
escaped at serialization: such a value would terminate its field early on the
wire and let a caller inject arbitrary fields.

## Limits

`parse-http-sse-events` and `read-http-sse-events` take `:max-events`,
`:max-line-bytes` and `:max-data-bytes`. Exceeding one signals
`sse-size-limit-exceeded`, which carries the limit, what was observed, and
which budget it was as separate slots — so a caller can decide whether to
raise the budget or drop the stream without parsing a message string.

## License

MIT. See [LICENSE](LICENSE).
