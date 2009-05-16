
;;;; Copyright (C) 2005 by Egil Möller . All Rights Reserved.
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining a
;;;; copy of this software and associated documentation files (the "Software"),
;;;; to deal in the Software without restriction, including without limitation
;;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;;; and/or sell copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be included
;;;; in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
;;;; DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

;;;; This is an implementation of an indentation reader for Scheme.
;;;; It is known to work on Guile and Gambit.
;;;; Original source code here: http://srfi.schemers.org/srfi-49/srfi-49.html

;;;; No warranty whatsoever, but feel free to send me an email at:
;;;; emailaliclark@gmail.com

;;;; Edited by Ali Clark:
;;;; Changed all symbols t to #t
;;;; Changed all symbols '. to (string->symbol ".")
;;;; Changed define-public to define
;;;; Changed sugar-read to call itself again if it reads nothing
;;;; Removed expressions containing tab character
;;;; Restructured document
;;;; Edited to allow forms to finish at an EOF
;;;; Edited to allow blank lines inside a form
;;;; Changed indentation model: rather than children being all elements
;;;;   equally indented to the first child of their parent,
;;;;   now any block is a child to a parent if it is more indented.
;;;;   This allows code like:
;;;;   if (test? bar)
;;;;       foo bar
;;;;     baz bar

(define sugar-group 'items)

(define sugar-read-save read)
(define sugar-load-save load)

;;; ADT

(define sugar-block-nil (string->symbol "."))

(define (sugar-block-null? block)
  (eq? block sugar-block-nil))

(define (sugar-block-unclean? block)
  (or (not (list? block)) (> (length block) 1)))

(define (sugar-struct parentlevel level block)
  (list 'sugar-struct parentlevel level block))

(define (sugar-struct-level struct) (caddr struct))
(define (sugar-struct-block struct) (cadddr struct))
(define (sugar-struct-eof? struct) (eof-object? (sugar-struct-block struct)))

(define (sugar-readquote port qt)
  (read-char port)
  (let ((char (peek-char port)))
    (if (or (eq? char #\space)
            (eq? char #\newline))
      (list qt)
      (list qt (sugar-read-save port)))))

(define (sugar-readitem port)
  (let ((char (peek-char port)))
    (cond
      ((eq? char #\`) (sugar-readquote port 'quasiquote))
      ((eq? char #\') (sugar-readquote port 'quote))
      ((eq? char #\,) (sugar-readquote port 'unquote))
      (else (sugar-read-save port)))))

(define (sugar-indentation>? indentation1 indentation2)
  (or (not (string? indentation2))
    (let ((len1 (string-length indentation1))
           (len2 (string-length indentation2)))
      (and (> len1 len2)
        (string=? indentation2 (substring indentation1 0 len2))))))

(define (sugar-indentationlevel port)
  (define (indentationlevel)
    (if (eq? (peek-char port) #\space)
      (cons (read-char port) (indentationlevel))
      '()))
  (let ((levl (indentationlevel)))
    (if (eq? (peek-char port) #\;)
      (begin
        (sugar-scrap-line port)
        (read-char port)
        (sugar-indentationlevel port))
      (list->string levl))))

(define (sugar-scrap-line port)
  (let ((char (peek-char port)))
    (if (eq? char #\newline)
      #t
      (begin
        (read-char port)
        (sugar-scrap-line port)))))

;; The next indentation level, disregarding blank lines.
(define (sugar-read-blanks level port)
  (let ((char (peek-char port)))
    (cond
      ((eof-object? char) "")
      ((eq? char #\space)
        (sugar-read-blanks (sugar-indentationlevel port) port))
      ((eq? char #\newline)
        (read-char port)
        (sugar-read-blanks (sugar-indentationlevel port) port))
      ((eq? char #\;)
        (sugar-scrap-line port)
        (sugar-read-blanks (sugar-indentationlevel port) port))
      (else level))))

(define (sugar-clean line)
  (cond
    ((or (not (pair? line)) (null? line)) line)
    ((or (eq? (car line) sugar-group) (null? (car line))) (cdr line))
    ((list? (car line))
      (if (or (equal? (car line) '(quote))
              (equal? (car line) '(quasiquote))
              (equal? (car line) '(unquote)))
        (if (and (list? (cdr line)) (= (length (cdr line)) 1))
          (cons (car (car line)) (cdr line))
          (list (car (car line)) (cdr line)))
        (cons (sugar-clean (car line)) (cdr line))))
    (else line)))

;; Reads all subblocks of a block
(define (sugar-readblocks parentlevel level port filep)
  (let* ((read       (sugar-readblock-clean parentlevel level port filep))
         (next-level (sugar-struct-level read))
         (block      (sugar-struct-block read)))
    (if (eof-object? block)
      (sugar-struct level next-level '())
      (if (sugar-indentation>? next-level parentlevel)
        (let* ((reads (sugar-readblocks parentlevel next-level port filep))
               (next-next-level (sugar-struct-level reads))
               (next-blocks     (sugar-struct-block reads)))
          (if (sugar-block-null? block)
            (if (pair? next-blocks)
              (sugar-struct next-level next-next-level (car next-blocks))
              (sugar-struct next-level next-next-level next-blocks))
            (sugar-struct next-level next-next-level (cons block next-blocks))))
        (sugar-struct level next-level (list block))))))

(define (sugar-check-get-more parentlevel level port filep)
  (read-char port)
  (let ((next
          (if filep (sugar-read-blanks level port)
            (sugar-indentationlevel port))))
    (if (sugar-indentation>? next level)
      (sugar-readblocks level next port filep)
      (sugar-struct level next '()))))

;; Read one block of input
(define (sugar-readblock parentlevel level port filep)
  (let ((char (peek-char port)))
    (cond
      ((eof-object? char)   (sugar-struct parentlevel -1 char))
      ((eq? char #\newline) (sugar-check-get-more parentlevel level port filep))
      ((eq? char #\space)
        (read-char port)
        (sugar-readblock parentlevel level port filep))
      ((eq? char #\;)
        (sugar-scrap-line port)
        (sugar-check-get-more parentlevel level port filep))
      (else
        (let* ((first  (sugar-readitem port))
               (rest   (sugar-readblock parentlevel level port filep))
               (level2 (sugar-struct-level rest))
               (block  (sugar-struct-block rest)))
          (if (sugar-block-null? first)
            (if (pair? block)
              (sugar-struct level level2 (car block))
              rest)
            (if (sugar-struct-eof? rest)
              (sugar-struct parentlevel level (cons first '()))
              (sugar-struct level level2 (cons first block)))))))))

;; reads a block and handles group, (quote), (unquote) and (quasiquote).
(define (sugar-readblock-clean parentlevel level port filep)
  (let* ((struct     (sugar-readblock parentlevel level port filep))
         (next-level (sugar-struct-level struct))
         (block      (sugar-struct-block struct)))
    (if (sugar-block-unclean? block)
      (sugar-struct level next-level (sugar-clean block))
      (if (= (length block) 1)
        (sugar-struct level next-level (car block))
        (sugar-struct level next-level sugar-block-nil)))))

(define (sugar-read-style filep . port)
  (let* ((p (if (null? port) (current-input-port) (car port)))
         (struct (sugar-readblock-clean #f (sugar-indentationlevel p) p filep))
         (block (sugar-struct-block struct)))
    (cond
      ((sugar-block-null? block) (apply sugar-read-style filep port))
      (else block))))

(define (sugar-read . port)      (apply sugar-read-style #f port))
(define (sugar-read-file . port) (apply sugar-read-style #t port))

(define (sugar-load filename)
  (define (load port)
    (let ((inp (sugar-read-style #t port)))
      (if (eof-object? inp) #t (begin (eval inp) (load port)))))
  (load (open-input-file filename)))

(define (sugar-enable)
  (set! read sugar-read)
  (set! load sugar-load))

(define (sugar-disable)
  (set! read sugar-read-save)
  (set! load sugar-load-save))

