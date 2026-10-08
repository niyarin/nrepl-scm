;;; session.scm -- nREPL session management
;;;
;;; SPDX-License-Identifier: MIT

(define-library (nrepl session)
  (export make-session-manager
          create-session!
          clone-session!
          close-session!
          get-session
          list-sessions
          clear-all-sessions!
          session-id
          session-namespace
          session-set-namespace!
          session-bindings
          session-set-binding!
          session-get-binding)

  (import (scheme base)
          (srfi 1))

  (cond-expand
   (guile
    (import (only (guile)
                  gettimeofday
                  getpid
                  make-hash-table
                  hash-ref
                  hash-set!
                  hash-remove!
                  hash-map->list
                  hash-clear!)))
   (gauche
    (import (only (gauche base)
                  sys-time
                  sys-getpid
                  make-hash-table
                  hash-table-ref/default
                  hash-table-set!
                  hash-table-delete!
                  hash-table-for-each
                  hash-table-clear!))))

  (begin

    (cond-expand
     (guile
      (define (ht-make)              (make-hash-table))
      (define (ht-ref ht key dflt)   (hash-ref ht key dflt))
      (define (ht-set! ht key val)   (hash-set! ht key val))
      (define (ht-remove! ht key)    (hash-remove! ht key))
      (define (ht-for-each ht proc)  (hash-map->list (lambda (k v) (proc k v)) ht))
      (define (ht-keys ht)           (hash-map->list (lambda (k v) k) ht))
      (define (ht-clear! ht)         (hash-clear! ht))
      (define (now-seconds)          (car (gettimeofday)))
      (define (process-id)           (getpid))
      (define default-namespace      "(guile-user)"))
     (gauche
      (define (ht-make)              (make-hash-table 'equal?))
      (define (ht-ref ht key dflt)   (hash-table-ref/default ht key dflt))
      (define (ht-set! ht key val)   (hash-table-set! ht key val))
      (define (ht-remove! ht key)    (hash-table-delete! ht key))
      (define (ht-for-each ht proc)  (hash-table-for-each ht proc))
      (define (ht-keys ht)           (map car (hash-table->alist ht)))
      (define (ht-clear! ht)         (hash-table-clear! ht))
      (define (now-seconds)          (sys-time))
      (define (process-id)           (sys-getpid))
      (define default-namespace      "gauche.user")))

    ;; Session record type
    (define-record-type <session>
      (make-session-record id namespace bindings)
      session?
      (id session-id)
      (namespace session-namespace session-set-namespace!)
      (bindings session-bindings session-set-bindings!))

    ;; Session manager record type
    (define-record-type <session-manager>
      (make-session-manager-record sessions counter)
      session-manager?
      (sessions manager-sessions)
      (counter manager-counter set-manager-counter!))

    (define (make-session-manager)
      (make-session-manager-record (ht-make) 0))

    (define (generate-session-id manager)
      (let* ((count (manager-counter manager))
             (id (string-append
                  (number->string (process-id) 16)
                  "-"
                  (number->string (now-seconds) 16)
                  "-"
                  (number->string count 16))))
        (set-manager-counter! manager (+ count 1))
        id))

    (define (create-session! manager)
      (let* ((id (generate-session-id manager))
             (session (make-session-record id default-namespace (ht-make))))
        (ht-set! (manager-sessions manager) id session)
        session))

    (define (clone-session! manager session-id)
      (if session-id
          (let ((existing (ht-ref (manager-sessions manager) session-id #f)))
            (if existing
                (let* ((new-id (generate-session-id manager))
                       (new-session (make-session-record
                                     new-id
                                     (session-namespace existing)
                                     (ht-make))))
                  (ht-for-each (session-bindings existing)
                               (lambda (k v) (ht-set! (session-bindings new-session) k v)))
                  (ht-set! (manager-sessions manager) new-id new-session)
                  new-session)
                (create-session! manager)))
          (create-session! manager)))

    (define (close-session! manager session-id)
      (ht-remove! (manager-sessions manager) session-id))

    (define (get-session manager session-id)
      (ht-ref (manager-sessions manager) session-id #f))

    (define (list-sessions manager)
      (ht-keys (manager-sessions manager)))

    (define (clear-all-sessions! manager)
      (ht-clear! (manager-sessions manager)))

    (define (session-set-binding! session key value)
      (ht-set! (session-bindings session) key value))

    (define (session-get-binding session key default)
      (ht-ref (session-bindings session) key default))))
