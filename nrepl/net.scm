;;; net.scm -- TCP networking abstraction
;;;
;;; SPDX-License-Identifier: MIT

(define-library (nrepl net)
  (export tcp-server-create
          tcp-server-local-port
          tcp-server-accept
          tcp-server-close
          connection-input-port
          connection-output-port
          connection-close)

  (import (scheme base))

  (cond-expand
   (guile
    (import (only (guile)
                  socket bind listen accept close-port setsockopt getsockname
                  sockaddr:port AF_INET SOCK_STREAM SOL_SOCKET SO_REUSEADDR
                  INADDR_LOOPBACK)))
   (gauche
    (import (gauche base)
            (gauche net))))

  (begin

    (cond-expand
     (guile
      (define (tcp-server-create port)
        (let ((sock (socket AF_INET SOCK_STREAM 0)))
          (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
          (bind sock AF_INET INADDR_LOOPBACK port)
          (listen sock 5)
          sock))

      (define (tcp-server-local-port server)
        (sockaddr:port (getsockname server)))

      (define (tcp-server-accept server)
        (car (accept server)))

      (define (tcp-server-close server)
        (close-port server))

      (define (connection-input-port conn)  conn)
      (define (connection-output-port conn) conn)
      (define (connection-close conn)       (close-port conn)))

     (gauche
      (define (tcp-server-create port)
        (make-server-socket 'inet port :reuse-addr? #t))

      (define (tcp-server-local-port server)
        (sockaddr-port (socket-getsockname server)))

      (define (tcp-server-accept server)
        (socket-accept server))

      (define (tcp-server-close server)
        (socket-close server))

      (define (connection-input-port conn)  (socket-input-port conn :buffering :none))
      (define (connection-output-port conn) (socket-output-port conn :buffering :none))
      (define (connection-close conn)       (socket-close conn))))))
