;;; eval.scm -- nREPL expression evaluation
;;;
;;; SPDX-License-Identifier: MIT

(define-library (nrepl eval)
  (export eval-in-session)

  (import (scheme base)
          (scheme read)
          (scheme write)
          (scheme eval)
          (nrepl session))

  (cond-expand
   (guile
    (import (srfi 1)
            (only (guile)
                  resolve-module
                  resolve-interface
                  *unspecified*)))
   (gauche
    (import (gauche base))))

  (begin

    (cond-expand
     (guile
      (define (namespace->module ns-name)
        (let* ((port (open-input-string ns-name))
               (name (read port)))
          (unless (and (list? name)
                       (not (null? name))
                       (every symbol? name)
                       (eof-object? (read port)))
            (error "Invalid Scheme library name" ns-name))
          (resolve-interface name)
          (resolve-module name)))

      (define empty-result *unspecified*))

     (gauche
      (define (namespace->module ns-name)
        (find-module (string->symbol ns-name)))

      (define empty-result (undefined))))

    (define (eval-in-session code session requested-ns)
      "Evaluate code string in session context.
Returns (cons 'ok value-string) or (cons 'error error-string)."
      (guard (exn (#t
                   (let ((p (open-output-string)))
                     (display exn p)
                     (cons 'error (get-output-string p)))))
        (let* ((ns-name (or requested-ns (session-namespace session)))
               (module (namespace->module ns-name)))
          (when requested-ns
            (session-set-namespace! session requested-ns))
          (let* ((port (open-input-string code))
                 (result (let loop ((last empty-result))
                           (let ((expr (read port)))
                             (if (eof-object? expr)
                                 last
                                 (loop (eval expr module)))))))
            (let ((p (open-output-string)))
              (write result p)
              (cons 'ok (get-output-string p)))))))))
