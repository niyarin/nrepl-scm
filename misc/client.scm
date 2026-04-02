#!/usr/bin/env guile
!#
;;; nrepl-client.scm -- Interactive nREPL client
;;; SPDX-License-Identifier: MIT

(add-to-load-path (dirname (current-filename)))

(use-modules (nrepl bencode)
             (ice-9 readline)
             (ice-9 format)
             (ice-9 rdelim))

;; Enable readline
(activate-readline)

;; ANSI colors
(define (color code str)
  (format #f "\x1b[~am~a\x1b[0m" code str))

(define (green str) (color 32 str))
(define (red str) (color 31 str))
(define (yellow str) (color 33 str))
(define (cyan str) (color 36 str))
(define (gray str) (color 90 str))

;; Connection
(define (connect-nrepl host port)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (connect sock AF_INET (inet-pton AF_INET host) port)
    sock))

;; Request/Response
(define (send-request sock request)
  (bencode-write request sock)
  (force-output sock))

(define (read-response sock)
  (bencode-read sock))

(define (alist-get key alist default)
  (let ((pair (assoc key alist)))
    (if pair (cdr pair) default)))

;; nREPL operations
(define (nrepl-clone sock)
  (send-request sock '(("op" . "clone")))
  (let ((response (read-response sock)))
    (alist-get "new-session" response #f)))

(define (nrepl-eval sock session-id code)
  (send-request sock `(("op" . "eval")
                       ("session" . ,session-id)
                       ("code" . ,code)))
  ;; Read all responses until "done"
  (let loop ((responses '()))
    (let* ((response (read-response sock))
           (status (alist-get "status" response '())))
      (if (member "done" status)
          (reverse (cons response responses))
          (loop (cons response responses))))))

(define (nrepl-describe sock)
  (send-request sock '(("op" . "describe")))
  (read-response sock))

;; Display response
(define (display-responses responses)
  (for-each
   (lambda (resp)
     (let ((value (alist-get "value" resp #f))
           (out (alist-get "out" resp #f))
           (err (alist-get "err" resp #f))
           (ex (alist-get "ex" resp #f))
           (ns (alist-get "ns" resp #f)))
       (when out
         (display out))
       (when err
         (display (red err)))
       (when ex
         (display (red (format #f "Error: ~a~%" ex))))
       (when value
         (display (green value))
         (newline))))
   responses))

;; Check if input is complete (balanced parens)
(define (balanced? str)
  (let loop ((chars (string->list str))
             (depth 0)
             (in-string? #f)
             (escape? #f))
    (if (null? chars)
        (and (zero? depth) (not in-string?))
        (let ((c (car chars)))
          (cond
           (escape?
            (loop (cdr chars) depth in-string? #f))
           ((char=? c #\\)
            (loop (cdr chars) depth in-string? #t))
           ((char=? c #\")
            (loop (cdr chars) depth (not in-string?) #f))
           (in-string?
            (loop (cdr chars) depth in-string? #f))
           ((char=? c #\()
            (loop (cdr chars) (+ depth 1) in-string? #f))
           ((char=? c #\))
            (loop (cdr chars) (- depth 1) in-string? #f))
           (else
            (loop (cdr chars) depth in-string? #f)))))))

;; Read potentially multi-line input
(define (read-input prompt continue-prompt)
  (let loop ((acc ""))
    (let ((line (readline (if (string=? acc "") prompt continue-prompt))))
      (cond
       ((eof-object? line)
        (if (string=? acc "") line acc))
       ((string=? line "")
        (if (string=? acc "")
            (loop acc)
            (let ((full (string-append acc "\n")))
              (if (balanced? full)
                  full
                  (loop full)))))
       (else
        (let ((full (if (string=? acc "")
                        line
                        (string-append acc "\n" line))))
          (if (balanced? full)
              full
              (loop full))))))))

;; Commands
(define (handle-command sock session-id cmd)
  (cond
   ((or (string=? cmd ",quit") (string=? cmd ",q"))
    'quit)
   ((or (string=? cmd ",help") (string=? cmd ",h"))
    (display (cyan "Commands:\n"))
    (display "  ,help  ,h   - Show this help\n")
    (display "  ,quit  ,q   - Quit client\n")
    (display "  ,desc       - Describe server\n")
    (display "  ,session    - Show current session ID\n")
    (newline)
    'continue)
   ((string=? cmd ",desc")
    (let ((resp (nrepl-describe sock)))
      (format #t "~a~%" resp))
    'continue)
   ((string=? cmd ",session")
    (format #t "Session: ~a~%" (cyan session-id))
    'continue)
   (else
    (format #t "~a~%" (red (format #f "Unknown command: ~a" cmd)))
    'continue)))

;; Main REPL
(define (repl sock session-id)
  (let ((prompt (string-append (green "guile") "> "))
        (continue-prompt (string-append (gray "....") "> ")))
    (let loop ()
      (let ((input (read-input prompt continue-prompt)))
        (cond
         ((eof-object? input)
          (newline)
          (display "Goodbye!\n"))
         ((string=? (string-trim-both input) "")
          (loop))
         ((char=? (string-ref (string-trim input) 0) #\,)
          ;; Command
          (let ((result (handle-command sock session-id (string-trim input))))
            (unless (eq? result 'quit)
              (loop))))
         (else
          ;; Eval
          (let ((responses (nrepl-eval sock session-id input)))
            (display-responses responses)
            (loop))))))))

;; Entry point
(define (main args)
  (let* ((host "127.0.0.1")
         (port (if (and (pair? (cdr args))
                        (string->number (cadr args)))
                   (string->number (cadr args))
                   7888)))

    (format #t "Connecting to nREPL server at ~a:~a...~%" host port)

    (catch #t
      (lambda ()
        (let* ((sock (connect-nrepl host port))
               (session-id (nrepl-clone sock)))
          (format #t "Connected! Session: ~a~%~%" (cyan session-id))
          (display (gray "Type ,help for commands, ,quit to exit.\n\n"))
          (repl sock session-id)
          (close-port sock)))
      (lambda (key . args)
        (format #t "~a~%" (red (format #f "Failed to connect: ~a ~a" key args)))
        (exit 1)))))

;; Run
(main (command-line))
