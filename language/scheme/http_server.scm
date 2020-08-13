;;; HTTP server
(use-modules (web server))

(define (my-handler request request-body)
  (values '((content-type . (text/plain)))
          "Hello Scheme!"))

(run-server my-handler)
