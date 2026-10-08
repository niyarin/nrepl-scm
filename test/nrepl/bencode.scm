(cond-expand
  (guile (import (nrepl bencode)
                 (rnrs bytevectors)
                 (srfi srfi-1)
                 (srfi srfi-64)))
  (gauche (import (scheme base)
                  (scheme write)
                  (srfi 1)
                  (srfi 64)
                  (gauche base)
                  (nrepl bencode))))

(define (alist-equal? a b)
  (and (= (length a) (length b))
       (every (lambda (pair)
                (equal? (assoc (car pair) b) pair))
              a)))

(test-begin "bencode")

;; Integer encoding
(test-equal "encode positive integer"  "i42e"        (utf8->string (bencode-encode 42)))
(test-equal "encode zero"              "i0e"          (utf8->string (bencode-encode 0)))
(test-equal "encode negative integer"  "i-42e"        (utf8->string (bencode-encode -42)))
(test-equal "encode large integer"     "i123456789e"  (utf8->string (bencode-encode 123456789)))

;; String encoding
(test-equal "encode simple string"     "5:hello"        (utf8->string (bencode-encode "hello")))
(test-equal "encode empty string"      "0:"             (utf8->string (bencode-encode "")))
(test-equal "encode string with space" "11:hello world" (utf8->string (bencode-encode "hello world")))

;; List encoding
(test-equal "encode empty list"        "le"             (utf8->string (bencode-encode '())))
(test-equal "encode list of integers"  "li1ei2ei3ee"    (utf8->string (bencode-encode '(1 2 3))))
(test-equal "encode list of strings"   "l1:a1:be"       (utf8->string (bencode-encode '("a" "b"))))
(test-equal "encode mixed list"        "l5:helloi42ee"  (utf8->string (bencode-encode '("hello" 42))))

;; Dictionary encoding (keys sorted lexicographically)
(test-equal "encode simple dict"       "d1:ai1e1:bi2ee" (utf8->string (bencode-encode '(("a" . 1) ("b" . 2)))))
(test-equal "encode dict key ordering" "d1:ai1e1:bi2ee" (utf8->string (bencode-encode '(("b" . 2) ("a" . 1)))))
(test-equal "encode nested dict"       "d3:key5:valuee" (utf8->string (bencode-encode '(("key" . "value")))))

;; Roundtrip tests
(test-equal "roundtrip integer 0"      0          (bencode-decode (bencode-encode 0)))
(test-equal "roundtrip integer 42"     42         (bencode-decode (bencode-encode 42)))
(test-equal "roundtrip integer -100"   -100       (bencode-decode (bencode-encode -100)))
(test-equal "roundtrip empty string"   ""         (bencode-decode (bencode-encode "")))
(test-equal "roundtrip simple string"  "hello"    (bencode-decode (bencode-encode "hello")))
(test-equal "roundtrip unicode string" "日本語"   (bencode-decode (bencode-encode "日本語")))
(test-equal "roundtrip empty list"     '()        (bencode-decode (bencode-encode '())))
(test-equal "roundtrip list of integers" '(1 2 3) (bencode-decode (bencode-encode '(1 2 3))))
(test-equal "roundtrip nested list"    '((1 2) (3 4)) (bencode-decode (bencode-encode '((1 2) (3 4)))))

(test-assert "roundtrip dict"
  (alist-equal? '(("foo" . "bar") ("baz" . 42))
                (bencode-decode (bencode-encode '(("foo" . "bar") ("baz" . 42))))))

;; nREPL-style message roundtrip
(test-assert "roundtrip nREPL eval message"
  (alist-equal? '(("op" . "eval") ("code" . "(+ 1 2)") ("session" . "abc123"))
                (bencode-decode (bencode-encode '(("op" . "eval") ("code" . "(+ 1 2)") ("session" . "abc123"))))))

(test-assert "roundtrip nREPL clone message"
  (alist-equal? '(("id" . "1") ("op" . "clone"))
                (bencode-decode (bencode-encode '(("id" . "1") ("op" . "clone"))))))

(test-end "bencode")
