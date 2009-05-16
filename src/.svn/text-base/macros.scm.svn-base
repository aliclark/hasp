
;;;; Copyright (c) 2009 Ali Clark
;;;; 
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;; 
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.
;;;; 
;;;; Except as contained in this notice, the name(s) of the above
;;;; copyright holders shall not be used in advertising or otherwise to
;;;; promote the sale, use or other dealings in this Software without
;;;; prior written authorization.

(define-macro (if-list obj code)
  `(if (list? ,obj) ,code ,obj))

(define-macro (add-form! type name value)
  `(table-set! forms ',name (cons ',type ,value)))

(define-macro (defmac formals form)
  (if (pair? formals)
    `(defmac ,(car formals) (lambda ,(cdr formals) ,form))
    `(add-form! macro ,formals ,form)))

(define-macro (defstatement indent formals . forms)
  `(add-form! statement ,(car formals)
     (lambda ,(cons indent (cdr formals))
       (string-append ,@(map (lambda (x) `(to-str ,x)) forms)))))

(define-macro (defgeneral indent formals . forms)
  `(add-form! general ,(car formals)
     (lambda ,(cons indent (cdr formals))
       (string-append ,@(map (lambda (x) `(to-str ,x)) forms)))))

(define-macro (definfix . syms)
  `(begin
     ,@(map
         (lambda (sym)
           (let ((out (if (list? sym) (cadr sym) sym))
                 (in  (if (list? sym) (car  sym) sym)))
             `(add-form! infix ,in ',out)))
         syms)))

(define-macro (assert a b)
  `(if (equal? (top-compile ',a) ,b) #t
     (begin (error "assertion failed: " ',a) #f)))

