;;; server.scm -- nREPL server
;;;
;;; SPDX-License-Identifier: MIT

(define-library (nrepl server)
  (export make-nrepl-server
          nrepl-server-start
          nrepl-server-stop
          nrepl-server-port
          nrepl-server-running?)

  (import (scheme base)
          (scheme write)
          (scheme file)
          (srfi 18)
          (nrepl bencode)
          (nrepl session)
          (nrepl handler)
          (nrepl net))

  (begin

    (define-record-type <nrepl-server>
      (make-nrepl-server-record socket port host running? accept-thread sessions)
      nrepl-server?
      (socket nrepl-server-socket set-nrepl-server-socket!)
      (port nrepl-server-port set-nrepl-server-port!)
      (host nrepl-server-host)
      (running? nrepl-server-running? set-nrepl-server-running?!)
      (accept-thread nrepl-server-accept-thread set-nrepl-server-accept-thread!)
      (sessions nrepl-server-sessions set-nrepl-server-sessions!))

    (define (make-nrepl-server . args)
      (let ((port (if (and (pair? args) (number? (car args))) (car args) 0))
            (host "127.0.0.1"))
        (make-nrepl-server-record #f port host #f #f (make-session-manager))))

    (define (write-port-file port)
      (call-with-output-file ".nrepl-port"
        (lambda (out)
          (display port out)
          (newline out))))

    (define (delete-port-file)
      (when (file-exists? ".nrepl-port")
        (delete-file ".nrepl-port")))

    (define (startup-message host port)
      (let ((msg (string-append "nREPL server started on port "
                                (number->string port)
                                " on host " host
                                " - nrepl://" host ":" (number->string port))))
        (display msg)
        (newline)
        msg))

    (define (response-alist? obj)
      (and (pair? obj)
           (pair? (car obj))
           (string? (caar obj))))

    (define (send-response out-port response)
      (cond
       ((response-alist? response)
        (bencode-write response out-port))
       ((list? response)
        (for-each (lambda (resp) (bencode-write resp out-port)) response))
       (else
        (bencode-write response out-port))))

    (define (handle-client server connection)
      (let ((in-port  (connection-input-port connection))
            (out-port (connection-output-port connection)))
        (guard (exn (#t
                     (display "Error handling client: " (current-error-port))
                     (display exn (current-error-port))
                     (newline (current-error-port))
                     #f))
          (let loop ()
            (when (nrepl-server-running? server)
              (let ((byte (peek-u8 in-port)))
                (unless (eof-object? byte)
                  (let* ((request  (bencode-read in-port))
                         (response (handle-request (nrepl-server-sessions server) request)))
                    (send-response out-port response)
                    (loop)))))))
        (connection-close connection)))

    (define (accept-loop server)
      (let ((sock (nrepl-server-socket server)))
        (let loop ()
          (when (nrepl-server-running? server)
            (guard (exn (#t #f))
              (let ((conn (tcp-server-accept sock)))
                (thread-start!
                 (make-thread (lambda () (handle-client server conn)))))
              (loop))))))

    (define (nrepl-server-start server)
      (let* ((sock        (tcp-server-create (nrepl-server-port server)))
             (actual-port (tcp-server-local-port sock)))
        (set-nrepl-server-port!     server actual-port)
        (set-nrepl-server-socket!   server sock)
        (set-nrepl-server-running?! server #t)
        (write-port-file actual-port)
        (startup-message (nrepl-server-host server) actual-port)
        (let ((thread (make-thread (lambda () (accept-loop server)))))
          (thread-start! thread)
          (set-nrepl-server-accept-thread! server thread))
        server))

    (define (nrepl-server-stop server)
      (when (nrepl-server-running? server)
        (set-nrepl-server-running?! server #f)
        (when (nrepl-server-socket server)
          (tcp-server-close (nrepl-server-socket server))
          (set-nrepl-server-socket! server #f))
        (delete-port-file)
        (clear-all-sessions! (nrepl-server-sessions server))
        server))))
