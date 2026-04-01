#!/usr/bin/env guile
!#
;;; test-bencode.scm -- Tests for bencode encoder/decoder
;;; SPDX-License-Identifier: MIT

(add-to-load-path (dirname (current-filename)))

(use-modules (nrepl bencode)
             (rnrs bytevectors)
             (srfi srfi-1)
             (ice-9 format))

(define (test name expected actual)
  (if (equal? expected actual)
      (format #t "PASS: ~a~%" name)
      (format #t "FAIL: ~a~%  expected: ~s~%  actual:   ~s~%" name expected actual)))

(define (alist-equal? a b)
  "Compare two alists ignoring order."
  (and (= (length a) (length b))
       (every (lambda (pair)
                (equal? (assoc (car pair) b) pair))
              a)))

(define (test-roundtrip name obj)
  "Test that encoding then decoding returns the original object."
  (let* ((encoded (bencode-encode obj))
         (decoded (bencode-decode encoded)))
    (test (format #f "roundtrip: ~a" name) obj decoded)))

(define (test-roundtrip-dict name obj)
  "Test roundtrip for dicts (ignoring key order)."
  (let* ((encoded (bencode-encode obj))
         (decoded (bencode-decode encoded)))
    (if (alist-equal? obj decoded)
        (format #t "PASS: roundtrip: ~a~%" name)
        (format #t "FAIL: roundtrip: ~a~%  expected: ~s~%  actual:   ~s~%" name obj decoded))))

(define (test-encode name obj expected-str)
  "Test that encoding produces the expected string."
  (let ((encoded (utf8->string (bencode-encode obj))))
    (test (format #f "encode: ~a" name) expected-str encoded)))

(format #t "~%=== Bencode Tests ===~%~%")

;; Integer encoding
(test-encode "positive integer" 42 "i42e")
(test-encode "zero" 0 "i0e")
(test-encode "negative integer" -42 "i-42e")
(test-encode "large integer" 123456789 "i123456789e")

;; String encoding
(test-encode "simple string" "hello" "5:hello")
(test-encode "empty string" "" "0:")
(test-encode "string with space" "hello world" "11:hello world")

;; List encoding
(test-encode "empty list" '() "le")
(test-encode "list of integers" '(1 2 3) "li1ei2ei3ee")
(test-encode "list of strings" '("a" "b") "l1:a1:be")
(test-encode "mixed list" '("hello" 42) "l5:helloi42ee")

;; Dictionary encoding (keys sorted lexicographically)
(test-encode "simple dict" '(("a" . 1) ("b" . 2)) "d1:ai1e1:bi2ee")
(test-encode "dict key ordering" '(("b" . 2) ("a" . 1)) "d1:ai1e1:bi2ee")
(test-encode "nested dict" '(("key" . "value")) "d3:key5:valuee")

;; Roundtrip tests
(format #t "~%--- Roundtrip Tests ---~%")
(test-roundtrip "integer 0" 0)
(test-roundtrip "integer 42" 42)
(test-roundtrip "integer -100" -100)
(test-roundtrip "empty string" "")
(test-roundtrip "simple string" "hello")
(test-roundtrip "unicode string" "日本語")
(test-roundtrip "empty list" '())
(test-roundtrip "list of integers" '(1 2 3))
(test-roundtrip "nested list" '((1 2) (3 4)))
(test-roundtrip-dict "dict" '(("foo" . "bar") ("baz" . 42)))

;; nREPL-style message roundtrip
(format #t "~%--- nREPL Message Tests ---~%")
(let ((msg '(("op" . "eval")
             ("code" . "(+ 1 2)")
             ("session" . "abc123"))))
  (test-roundtrip-dict "nREPL eval message" msg))

(let ((msg '(("id" . "1")
             ("op" . "clone"))))
  (test-roundtrip-dict "nREPL clone message" msg))

(format #t "~%=== Done ===~%")
