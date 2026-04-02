;;; server.scm -- nREPL server for Guile
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
          (srfi 1)
          (nrepl bencode)
          (nrepl session)
          (nrepl handler))

  (cond-expand
   (guile
    (import (only (guile)
                  socket
                  bind
                  listen
                  accept
                  close-port
                  setsockopt
                  getsockname
                  current-output-port
                  current-error-port
                  with-exception-handler
                  format
                  sockaddr:port
                  file-exists?
                  delete-file
                  AF_INET
                  SOCK_STREAM
                  SOL_SOCKET
                  SO_REUSEADDR
                  INADDR_LOOPBACK)
            (only (ice-9 threads)
                  call-with-new-thread)
            (only (ice-9 binary-ports)
                  lookahead-u8))))

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
      "Create a new nREPL server.
Optional keyword arguments:
  port: Port number (default: 0 for auto-assign)
  host: Host to bind to (default: 127.0.0.1)"
      (let ((port (if (and (pair? args) (number? (car args)))
                      (car args)
                      0))
            (host "127.0.0.1"))
        (make-nrepl-server-record #f port host #f #f (make-session-manager))))

    (define (write-port-file port)
      "Write .nrepl-port file with the server port."
      (call-with-output-file ".nrepl-port"
        (lambda (out)
          (display port out)
          (newline out))))

    (define (delete-port-file)
      "Delete .nrepl-port file if it exists."
      (when (file-exists? ".nrepl-port")
        (delete-file ".nrepl-port")))

    (define (startup-message host port)
      "Print the nREPL startup message."
      (let ((msg (string-append "nREPL server started on port "
                                (number->string port)
                                " on host " host
                                " - nrepl://" host ":" (number->string port))))
        (display msg)
        (newline)
        msg))

    (define (response-alist? obj)
      "Check if obj is a response alist (not a list of responses)."
      (and (pair? obj)
           (pair? (car obj))
           (string? (caar obj))))

    (define (send-response client-socket response)
      "Send one or more responses to client."
      (cond
       ((response-alist? response)
        ;; Single response
        (bencode-write response client-socket))
       ((list? response)
        ;; Multiple responses
        (for-each (lambda (resp)
                    (bencode-write resp client-socket))
                  response))
       (else
        (bencode-write response client-socket))))

    (define (handle-client server client-socket client-addr)
      "Handle a single client connection."
      (with-exception-handler
          (lambda (exn)
            (format (current-error-port)
                    "Error handling client: ~a~%"
                    exn)
            #f)
        (lambda ()
          (let loop ()
            (when (nrepl-server-running? server)
              (let ((byte (lookahead-u8 client-socket)))
                (unless (eof-object? byte)
                  (let* ((request (bencode-read client-socket))
                         (response (handle-request (nrepl-server-sessions server)
                                                   request)))
                    (send-response client-socket response)
                    (loop))))))
          (close-port client-socket))
        #:unwind? #t))

    (define (accept-loop server)
      "Main accept loop for the server."
      (let ((sock (nrepl-server-socket server)))
        (let loop ()
          (when (nrepl-server-running? server)
            (with-exception-handler
                (lambda (exn)
                  ;; Server might be shutting down
                  #f)
              (lambda ()
                (let* ((client-pair (accept sock))
                       (client-socket (car client-pair))
                       (client-addr (cdr client-pair)))
                  (call-with-new-thread
                   (lambda ()
                     (handle-client server client-socket client-addr))))
                (loop))
              #:unwind? #t)))))

    (define (nrepl-server-start server)
      "Start the nREPL server."
      (let ((sock (socket AF_INET SOCK_STREAM 0)))
        ;; Allow port reuse
        (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
        ;; Bind to port
        (bind sock AF_INET INADDR_LOOPBACK (nrepl-server-port server))
        ;; Get actual port (if 0 was specified)
        (let ((actual-port (sockaddr:port (getsockname sock))))
          (set-nrepl-server-port! server actual-port)
          ;; Listen for connections
          (listen sock 5)
          (set-nrepl-server-socket! server sock)
          (set-nrepl-server-running?! server #t)
          ;; Write port file
          (write-port-file actual-port)
          ;; Print startup message
          (startup-message (nrepl-server-host server) actual-port)
          ;; Start accept thread
          (let ((thread (call-with-new-thread
                         (lambda () (accept-loop server)))))
            (set-nrepl-server-accept-thread! server thread))
          server)))

    (define (nrepl-server-stop server)
      "Stop the nREPL server."
      (when (nrepl-server-running? server)
        (set-nrepl-server-running?! server #f)
        ;; Close the server socket
        (when (nrepl-server-socket server)
          (close-port (nrepl-server-socket server))
          (set-nrepl-server-socket! server #f))
        ;; Delete port file
        (delete-port-file)
        ;; Clean up sessions
        (clear-all-sessions! (nrepl-server-sessions server))
        server))))
