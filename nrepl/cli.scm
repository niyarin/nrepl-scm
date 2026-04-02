;;; cli.scm -- nREPL server command-line interface
;;;
;;; SPDX-License-Identifier: MIT

(define-library (nrepl cli)
  (export run-nrepl-server
          main)

  (import (scheme base)
          (scheme process-context)
          (nrepl server))

  (cond-expand
   (guile
    (import (only (guile)
                  sleep
                  string->number
                  current-output-port
                  display
                  newline))))

  (begin

    (define (parse-port args)
      "Parse port from command line arguments."
      (if (and (pair? args)
               (pair? (cdr args))
               (string->number (cadr args)))
          (string->number (cadr args))
          7888))

    (define (run-nrepl-server port)
      "Start nREPL server on given port and block."
      (let ((server (make-nrepl-server port)))
        (nrepl-server-start server)
        (let loop ()
          (sleep 1)
          (when (nrepl-server-running? server)
            (loop)))
        (nrepl-server-stop server)))

    (define (main args)
      "Main entry point."
      (let ((port (parse-port args)))
        (run-nrepl-server port)))))
