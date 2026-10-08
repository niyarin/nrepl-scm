(use-modules (nrepl cli))

(define (main args)
  ((@ (nrepl cli) main) args))
