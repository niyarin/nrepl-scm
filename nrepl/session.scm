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
                  hash-clear!))))

  (begin

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
      "Create a new session manager."
      (make-session-manager-record (make-hash-table) 0))

    (define (generate-session-id manager)
      "Generate a unique session ID."
      (let* ((count (manager-counter manager))
             (time (car (gettimeofday)))
             (pid (getpid))
             (id (string-append
                  (number->string pid 16)
                  "-"
                  (number->string time 16)
                  "-"
                  (number->string count 16))))
        (set-manager-counter! manager (+ count 1))
        id))

    (define (create-session! manager)
      "Create a new session with default bindings."
      (let* ((id (generate-session-id manager))
             (session (make-session-record id
                                           "(guile-user)"
                                           (make-hash-table))))
        (hash-set! (manager-sessions manager) id session)
        session))

    (define (clone-session! manager session-id)
      "Clone an existing session or create a new one if session-id is #f."
      (if session-id
          (let ((existing (hash-ref (manager-sessions manager) session-id #f)))
            (if existing
                (let* ((new-id (generate-session-id manager))
                       (new-session (make-session-record
                                     new-id
                                     (session-namespace existing)
                                     (make-hash-table))))
                  ;; Copy bindings
                  (hash-map->list
                   (lambda (k v)
                     (hash-set! (session-bindings new-session) k v))
                   (session-bindings existing))
                  (hash-set! (manager-sessions manager) new-id new-session)
                  new-session)
                ;; Session not found, create new
                (create-session! manager)))
          ;; No session-id, create new
          (create-session! manager)))

    (define (close-session! manager session-id)
      "Close and remove a session."
      (hash-remove! (manager-sessions manager) session-id))

    (define (get-session manager session-id)
      "Get a session by ID."
      (hash-ref (manager-sessions manager) session-id #f))

    (define (list-sessions manager)
      "List all session IDs."
      (hash-map->list (lambda (k v) k) (manager-sessions manager)))

    (define (clear-all-sessions! manager)
      "Remove all sessions."
      (hash-clear! (manager-sessions manager)))

    (define (session-set-binding! session key value)
      "Set a binding in the session."
      (hash-set! (session-bindings session) key value))

    (define (session-get-binding session key default)
      "Get a binding from the session."
      (hash-ref (session-bindings session) key default))))
