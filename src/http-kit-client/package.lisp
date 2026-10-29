(defpackage #:sse-kit/http-kit-client
  (:use #:cl)
  (:export
   #:http-sse-subscription
   #:http-sse-subscription-p
   #:http-sse-subscription-stopped-p
   #:http-sse-subscription-finished-p
   #:http-sse-subscription-error
   #:http-sse-subscription-last-event-id
   #:http-sse-subscription-next-retry-delay
   #:subscribe-http-sse
   #:stop-http-sse-subscription
   #:wait-http-sse-subscription))
