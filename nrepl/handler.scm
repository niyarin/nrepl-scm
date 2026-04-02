;;; handler.scm -- nREPL request handler
;;;
;;; SPDX-License-Identifier: MIT

(define-library (nrepl handler)
  (export handle-request
          nrepl-version
          guile-nrepl-version)

  (import (scheme base)
          (scheme read)
          (scheme write)
          (scheme eval)
          (srfi 1)
          (nrepl session))

  (cond-expand
   (guile
    (import (only (guile)
                  resolve-module
                  module-ref
                  current-module
                  set-current-module
                  current-error-port
                  format
                  call-with-output-string
                  call-with-input-string
                  with-exception-handler
                  interaction-environment
                  *unspecified*
                  version))))

  (begin

    (define nrepl-version "0.1.0")
    (define guile-nrepl-version "0.1.0")

    ;; Helper to get value from alist
    (define (alist-get key alist default)
      (let ((pair (assoc key alist)))
        (if pair (cdr pair) default)))

    ;; Helper to create response
    (define (make-response id session-id . pairs)
      "Create a response alist."
      (let ((base (list (cons "id" (or id "unknown")))))
        (append base
                (if session-id
                    (list (cons "session" session-id))
                    '())
                (let loop ((p pairs) (acc '()))
                  (if (null? p)
                      (reverse acc)
                      (loop (cddr p)
                            (cons (cons (car p) (cadr p)) acc)))))))

    ;; Add status to response
    (define (with-status response . statuses)
      (append response (list (cons "status" statuses))))

    ;;; ============================================================
    ;;; Op handlers
    ;;; ============================================================

    (define (handle-clone manager request)
      "Handle 'clone' op - create or clone a session."
      (let* ((id (alist-get "id" request #f))
             (sess-id (alist-get "session" request #f))
             (new-session (clone-session! manager sess-id))
             (new-sess-id (session-id new-session)))
        (with-status
         (make-response id new-sess-id
                        "new-session" new-sess-id)
         "done")))

    (define (handle-close manager request)
      "Handle 'close' op - close a session."
      (let* ((id (alist-get "id" request #f))
             (sess-id (alist-get "session" request #f)))
        (when sess-id
          (close-session! manager sess-id))
        (with-status
         (make-response id sess-id)
         "done")))

    (define (handle-describe manager request)
      "Handle 'describe' op - describe server capabilities."
      (let* ((id (alist-get "id" request #f))
             (sess-id (alist-get "session" request #f)))
        (with-status
         (make-response id sess-id
                        "ops" (list
                               (cons "clone" '())
                               (cons "close" '())
                               (cons "describe" '())
                               (cons "eval" '())
                               (cons "load-file" '())
                               (cons "ls-sessions" '())
                               (cons "completions" '())
                               (cons "lookup" '()))
                        "versions" (list
                                    (cons "nrepl" (list (cons "version-string" nrepl-version)))
                                    (cons "guile" (list (cons "version-string" (version))))
                                    (cons "guile-nrepl" (list (cons "version-string" guile-nrepl-version)))))
         "done")))

    (define (handle-ls-sessions manager request)
      "Handle 'ls-sessions' op - list all sessions."
      (let* ((id (alist-get "id" request #f))
             (sess-id (alist-get "session" request #f)))
        (with-status
         (make-response id sess-id
                        "sessions" (list-sessions manager))
         "done")))

    (define (handle-eval manager request)
      "Handle 'eval' op - evaluate code."
      (let* ((id (alist-get "id" request #f))
             (sess-id (alist-get "session" request #f))
             (code (alist-get "code" request ""))
             (session (if sess-id
                          (get-session manager sess-id)
                          #f)))
        ;; If no session, create one
        (unless session
          (set! session (create-session! manager))
          (set! sess-id (session-id session)))

        (with-exception-handler
            (lambda (exn)
              ;; Return error response
              (list
               (with-status
                (make-response id sess-id
                               "ex" (call-with-output-string
                                     (lambda (p) (display exn p)))
                               "root-ex" (call-with-output-string
                                          (lambda (p) (display exn p))))
                "done")))
          (lambda ()
            (let* ((result (eval-string-in-session code session))
                   (value-str (call-with-output-string
                               (lambda (p)
                                 (write result p)))))
              ;; Return success response
              (list
               (make-response id sess-id
                              "value" value-str
                              "ns" (session-namespace session))
               (with-status
                (make-response id sess-id)
                "done"))))
          #:unwind? #t)))

    (define (eval-string-in-session code session)
      "Evaluate code string in session context."
      (let ((ns-name (session-namespace session)))
        ;; Read and evaluate the code
        (call-with-input-string code
          (lambda (port)
            (let loop ((result *unspecified*))
              (let ((expr (read port)))
                (if (eof-object? expr)
                    result
                    (loop (eval expr (interaction-environment))))))))))

    (define (handle-load-file manager request)
      "Handle 'load-file' op - load a file."
      (let* ((id (alist-get "id" request #f))
             (sess-id (alist-get "session" request #f))
             (file-content (alist-get "file" request ""))
             (file-name (alist-get "file-name" request "unknown"))
             (file-path (alist-get "file-path" request "")))
        ;; Treat as eval for now
        (handle-eval manager
                     (list (cons "id" id)
                           (cons "session" sess-id)
                           (cons "code" file-content)))))

    (define (handle-completions manager request)
      "Handle 'completions' op - provide completion candidates."
      (let* ((id (alist-get "id" request #f))
             (sess-id (alist-get "session" request #f))
             (prefix (alist-get "prefix" request "")))
        ;; Basic implementation - return empty list for now
        (with-status
         (make-response id sess-id
                        "completions" '())
         "done")))

    (define (handle-lookup manager request)
      "Handle 'lookup' op - lookup symbol info."
      (let* ((id (alist-get "id" request #f))
             (sess-id (alist-get "session" request #f))
             (sym (alist-get "sym" request "")))
        ;; Basic implementation
        (with-status
         (make-response id sess-id
                        "info" '())
         "done")))

    (define (handle-unknown manager request)
      "Handle unknown op."
      (let* ((id (alist-get "id" request #f))
             (sess-id (alist-get "session" request #f))
             (op (alist-get "op" request "unknown")))
        (with-status
         (make-response id sess-id
                        "error" (string-append "Unknown op: " op))
         "error" "unknown-op" "done")))

    ;;; ============================================================
    ;;; Main dispatch
    ;;; ============================================================

    (define (handle-request manager request)
      "Dispatch request to appropriate handler."
      (let ((op (alist-get "op" request #f)))
        (cond
         ((equal? op "clone") (handle-clone manager request))
         ((equal? op "close") (handle-close manager request))
         ((equal? op "describe") (handle-describe manager request))
         ((equal? op "ls-sessions") (handle-ls-sessions manager request))
         ((equal? op "eval") (handle-eval manager request))
         ((equal? op "load-file") (handle-load-file manager request))
         ((equal? op "completions") (handle-completions manager request))
         ((equal? op "lookup") (handle-lookup manager request))
         (else (handle-unknown manager request)))))))
