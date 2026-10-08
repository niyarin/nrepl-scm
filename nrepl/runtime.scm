;;; runtime.scm -- implementation-specific runtime information
;;;
;;; SPDX-License-Identifier: MIT

(define-library (nrepl runtime)
  (export runtime-name
          runtime-version
          runtime-load-paths
          nrepl-impl-version)

  (import (scheme base)
          (srfi 1))

  (cond-expand
   (guile
    (import (only (guile)
                  version
                  getcwd
                  %load-path)))
   (gauche
    (import (only (gauche base)
                  gauche-version
                  sys-getcwd
                  *load-path*))))

  (begin

    (define nrepl-impl-version "0.1.0")

    (cond-expand
     (guile
      (define runtime-name    "guile")
      (define runtime-version (version))
      (define (runtime-load-paths)
        (delete-duplicates (cons (getcwd) %load-path) string=?)))
     (gauche
      (define runtime-name    "gauche")
      (define runtime-version (gauche-version))
      (define (runtime-load-paths)
        (delete-duplicates (cons (sys-getcwd) *load-path*) string=?))))))
