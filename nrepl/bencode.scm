;;; bencode.scm -- Bencode encoder/decoder for nREPL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Bencode is the encoding used by BitTorrent and nREPL.
;;; Format:
;;;   - Strings: <length>:<data>  (e.g., "5:hello")
;;;   - Integers: i<number>e  (e.g., "i42e")
;;;   - Lists: l<items>e  (e.g., "l5:helloi42ee")
;;;   - Dictionaries: d<key><value>...e  (keys sorted lexicographically)

(define-library (nrepl bencode)
  (export bencode-encode
          bencode-decode
          bencode-write
          bencode-read
          scm->bencode
          bencode->scm)

  (import (scheme base)
          (srfi 1))

  ;; Guile-specific imports for sorting
  (cond-expand
   (guile
    (import (only (rnrs sorting)
                  list-sort)))
   (else
    (error "Unsupported Scheme implementation")))

  (begin

    ;;; ============================================================
    ;;; Constants
    ;;; ============================================================

    (define CHAR-i (char->integer #\i))
    (define CHAR-l (char->integer #\l))
    (define CHAR-d (char->integer #\d))
    (define CHAR-e (char->integer #\e))
    (define CHAR-colon (char->integer #\:))
    (define CHAR-minus (char->integer #\-))
    (define CHAR-0 (char->integer #\0))
    (define CHAR-9 (char->integer #\9))

    ;;; ============================================================
    ;;; Encoder
    ;;; ============================================================

    (define (digit? byte)
      (and (>= byte CHAR-0) (<= byte CHAR-9)))

    (define (encode-integer n port)
      (write-u8 CHAR-i port)
      (write-string (number->string n) port)
      (write-u8 CHAR-e port))

    (define (encode-string str port)
      (let ((bv (string->utf8 str)))
        (write-string (number->string (bytevector-length bv)) port)
        (write-u8 CHAR-colon port)
        (write-bytevector bv port)))

    (define (encode-bytevector bv port)
      (write-string (number->string (bytevector-length bv)) port)
      (write-u8 CHAR-colon port)
      (write-bytevector bv port))

    (define (encode-list lst port)
      (write-u8 CHAR-l port)
      (for-each (lambda (item) (bencode-write item port)) lst)
      (write-u8 CHAR-e port))

    (define (encode-vector vec port)
      (write-u8 CHAR-l port)
      (vector-for-each (lambda (item) (bencode-write item port)) vec)
      (write-u8 CHAR-e port))

    (define (key->string key)
      (cond
       ((string? key) key)
       ((symbol? key) (symbol->string key))
       (else (error "Invalid dictionary key type" key))))

    (define (encode-alist alist port)
      (let* ((normalized (map (lambda (pair)
                                (cons (key->string (car pair)) (cdr pair)))
                              alist))
             (sorted (list-sort (lambda (a b)
                                  (string<? (car a) (car b)))
                                normalized)))
        (write-u8 CHAR-d port)
        (for-each (lambda (pair)
                    (encode-string (car pair) port)
                    (bencode-write (cdr pair) port))
                  sorted)
        (write-u8 CHAR-e port)))

    (define (valid-dict-key? obj)
      (or (string? obj) (symbol? obj)))

    (define (alist? obj)
      (and (list? obj)
           (pair? obj)
           (every (lambda (elem)
                    (and (pair? elem)
                         (valid-dict-key? (car elem))))
                  obj)))

    (define (bencode-write obj port)
      (cond
       ((integer? obj) (encode-integer obj port))
       ((string? obj) (encode-string obj port))
       ((bytevector? obj) (encode-bytevector obj port))
       ((vector? obj) (encode-vector obj port))
       ((alist? obj) (encode-alist obj port))
       ((list? obj) (encode-list obj port))
       ((symbol? obj) (encode-string (symbol->string obj) port))
       (else (error "Cannot bencode encode object" obj))))

    (define (bencode-encode obj)
      (let ((port (open-output-bytevector)))
        (bencode-write obj port)
        (get-output-bytevector port)))

    (define (scm->bencode obj)
      (bencode-encode obj))

    ;;; ============================================================
    ;;; Decoder
    ;;; ============================================================

    (define (read-byte-safe port)
      (let ((byte (read-u8 port)))
        (when (eof-object? byte)
          (error "Unexpected end of input"))
        byte))

    (define (peek-byte-safe port)
      (let ((byte (peek-u8 port)))
        (when (eof-object? byte)
          (error "Unexpected end of input"))
        byte))

    (define (read-integer-bytes port delim)
      (let loop ((acc 0)
                 (negate? #f)
                 (first? #t))
        (let ((byte (read-byte-safe port)))
          (cond
           ((= byte delim)
            (if negate? (- acc) acc))
           ((and first? (= byte CHAR-minus))
            (loop acc #t #f))
           ((digit? byte)
            (loop (+ (* acc 10) (- byte CHAR-0)) negate? #f))
           (else
            (error "Invalid character in integer" (integer->char byte)))))))

    (define (decode-integer port)
      (read-integer-bytes port CHAR-e))

    (define (decode-string port)
      (let* ((len (read-integer-bytes port CHAR-colon))
             (bv (read-bytevector len port)))
        (when (or (eof-object? bv) (< (bytevector-length bv) len))
          (error "Unexpected end of input while reading string"))
        (utf8->string bv)))

    (define (decode-string-starting-with port first-byte)
      (let loop ((len (- first-byte CHAR-0)))
        (let ((byte (read-byte-safe port)))
          (cond
           ((= byte CHAR-colon)
            (if (zero? len)
                ""
                (let ((bv (read-bytevector len port)))
                  (when (or (eof-object? bv) (< (bytevector-length bv) len))
                    (error "Unexpected end of input while reading string"))
                  (utf8->string bv))))
           ((digit? byte)
            (loop (+ (* len 10) (- byte CHAR-0))))
           (else
            (error "Invalid character in string length" (integer->char byte)))))))

    (define (decode-list port)
      (let loop ((acc '()))
        (let ((byte (peek-byte-safe port)))
          (if (= byte CHAR-e)
              (begin
                (read-u8 port)
                (reverse acc))
              (loop (cons (bencode-read port) acc))))))

    (define (decode-dict port)
      (let loop ((acc '()))
        (let ((byte (peek-byte-safe port)))
          (if (= byte CHAR-e)
              (begin
                (read-u8 port)
                (reverse acc))
              (let* ((key (bencode-read port))
                     (val (bencode-read port)))
                (loop (cons (cons key val) acc)))))))

    (define (bencode-read port)
      (let ((byte (read-byte-safe port)))
        (cond
         ((= byte CHAR-i) (decode-integer port))
         ((= byte CHAR-l) (decode-list port))
         ((= byte CHAR-d) (decode-dict port))
         ((digit? byte) (decode-string-starting-with port byte))
         (else (error "Invalid bencode token" (integer->char byte))))))

    (define (bencode-decode bv)
      (call-with-port (open-input-bytevector bv) bencode-read))

    (define (bencode->scm bv)
      (bencode-decode bv))))
