(cond-expand
  (guile (import (nrepl eval)
                 (prefix (nrepl session) session/)
                 (srfi srfi-64)))
  (gauche (import (scheme base)
                  (srfi 64)
                  (nrepl eval)
                  (prefix (nrepl session) session/))))

(cond-expand
  (guile  (define default-ns "(guile-user)"))
  (gauche (define default-ns "gauche.user")))

(define (fresh-session)
  (let ((mgr (session/make-session-manager)))
    (session/create-session! mgr)))

(test-begin "eval")

;; Basic evaluation
(test-equal "single expression"
  'ok
  (car (eval-in-session "(+ 1 2)" (fresh-session) #f)))

(test-equal "single expression value"
  "3"
  (cdr (eval-in-session "(+ 1 2)" (fresh-session) #f)))

(test-equal "multiple expressions returns last"
  "12"
  (cdr (eval-in-session "(+ 1 2) (* 3 4)" (fresh-session) #f)))

(test-equal "empty string returns ok"
  'ok
  (car (eval-in-session "" (fresh-session) #f)))

;; Value write format
(test-equal "write integer"  "42"        (cdr (eval-in-session "42"        (fresh-session) #f)))
(test-equal "write boolean"  "#t"        (cdr (eval-in-session "#t"        (fresh-session) #f)))
(test-equal "write string"   "\"hello\"" (cdr (eval-in-session "\"hello\"" (fresh-session) #f)))
(test-equal "write list"     "(1 2 3)"   (cdr (eval-in-session "'(1 2 3)"  (fresh-session) #f)))

;; Error handling
(test-equal "exception returns error tag"
  'error
  (car (eval-in-session "(error \"oops\")" (fresh-session) #f)))

(test-assert "error message is non-empty string"
  (let ((r (eval-in-session "(error \"oops\")" (fresh-session) #f)))
    (and (string? (cdr r))
         (> (string-length (cdr r)) 0))))

(test-equal "session usable after error"
  'ok
  (let ((sess (fresh-session)))
    (eval-in-session "(error \"oops\")" sess #f)
    (car (eval-in-session "(+ 1 2)" sess #f))))

;; Namespace management
(test-equal "requested-ns updates session-namespace"
  default-ns
  (let ((sess (fresh-session)))
    (eval-in-session "(+ 1 2)" sess default-ns)
    (session/session-namespace sess)))

(test-equal "requested-ns #f does not change namespace"
  default-ns
  (let ((sess (fresh-session)))
    (session/session-set-namespace! sess default-ns)
    (eval-in-session "(+ 1 2)" sess #f)
    (session/session-namespace sess)))

(test-end "eval")
