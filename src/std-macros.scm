
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

(define (take n l)
  (if (zero? n)
    (list)
    (cons (car l) (take (dec n) (cdr l)))))

(define (skip n l)
  (if (zero? n)
    l
    (skip (dec n) (cdr l))))

(define (group n l)
  (if (< (length l) n)
    (list)
    (cons (take n l) (group n (skip n l)))))

(define (zip xs ys)
  (if (or (null? xs) (null? ys))
    (list)
    (cons (cons (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))

(define (repeat-call f n)
  (if (zero? n)
    (list)
    (cons (f) (repeat-call f (dec n)))))

(define (cond-item x)
  (if (null? (cddr x))
    `(tup ,(car x) ,(cadr x))
    `(tup ,(car x) (let ,(butlast (cdr x)) ,(last (cdr x))))))

(define (blank-fill lst var)
  (if (null? lst)
    (list)
    (cons (if (eq? (car lst) var) var '_) (blank-fill (cdr lst) var))))

(define (unblank-fill lst var fill)
  (if (null? lst)
    (list)
    (cons (if (eq? (car lst) var) fill (car lst))
      (unblank-fill (cdr lst) var fill))))

(define (build-accessors accessorp name . fields)
  (let ((symbols (map (lambda (x) (if (eq? x '_) (gens) x)) fields)))
    `(begin
       ,@(let loop ((acc (list)) (rest fields))
           (if (null? rest)
             (reverse acc)
             (loop
               (let ((varname (car rest)))
                 (if (eq? varname '_)
                   acc
                   (cons
                     (let ((getter
                             `(fn ((,name ,@(blank-fill symbols varname)))
                                ,varname)))
                       `(def ,varname
                          ,(if accessorp
                             `(tup
                                ,getter
                                (fn ((,name ,@(unblank-fill symbols varname '_)) ,varname)
                                  (,name
                                    ,@(unblank-fill symbols varname varname))))
                             getter)))
                     acc)))
               (cdr rest)))))))

;;;; These are the standard "idiomatic" macros of hasp.
;;;; Contributions very welcome.

(defmac (= . forms)  `(def ,@forms))
(defmac (if . forms) `(if- ,@forms))

(defmac (list . items)
  (if (null? items)
    'Nil
    `(: ,(car items) (list ,@(cdr items)))))

(defmac (deft formals typex . forms)
  `(begin
     ,(if (null? typex) '(begin)
        `(:: ,(if (list? formals) (car formals) formals) ,typex))
     (def ,formals ,@forms)))

(defmac (defn name typex . definitions)
  (let ((vars (map (lambda (x) (gens 'v)) (caar definitions))))
    `(begin ,(if (null? typex) '(begin) `(:: ,name ,typex))
       (def (,name ,@vars)
         (case-of (tup ,@vars)
           ,@(map
               (lambda (definition)
                 (cons (cons 'tup (car definition)) (cdr definition)))
               definitions))))))

(defmac (let= definitions . forms)
  `(let ,(map (lambda (x) `(def ,(car x) ,@(cdr x))) (group 2 definitions))
    ,@forms))

(defmac (letl formal . body)
  `(let ((def ,(map (lambda (x) (if-list x (car x))) formal) ,@body))
     ,(map (lambda (x) (if-list x (if (null? (cdr x)) (car x) (cadr x))))
       formal)))

(defmac (let>>= definitions . forms)
  (if (or (null? definitions) (null? (cdr definitions)))
    `(let ,(butlast forms) ,(last forms))
    `(>>= ,(cadr definitions)
      (fn (,(car definitions)) (let>>= ,(cddr definitions) ,@forms)))))

(defmac (cond . tests)
  `(condList (list ,@(map cond-item (butlast tests))) ,(last tests)))

(defmac (case find . forms)
  `(caseList ,find (list ,@(map cond-item (butlast forms))) ,(last forms)))

(defmac defaccessors (partial build-accessors #t))
(defmac defgetters   (partial build-accessors #f))

;; Deriving instances for data.

;; Do we also need to use the context from the data definition somehow?
(define (derive-class class fn last-case combiner)
  (lambda (formal . parts)
    `(instance (=> (,@(map (lambda (var) `(,class ,var)) (cdr formal)))
                 (,class ,formal))
       ,@(map
           (lambda (part)
             (let* ((constr (car part))
                    (varcount (length (cdr part)))
                    (xs (repeat-call (partial gens 'x) varcount))
                    (ys (repeat-call (partial gens 'y) varcount))
                    (pairs (zip xs ys)))
               `(def (,fn (,constr ,@xs) (,constr ,@ys))
                  ,(if (> (length xs) 1)
                     `(,combiner
                        ,@(map (lambda (p) `(,fn ,(car p) ,(cdr p))) pairs))
                     `(,fn ,(caar pairs) ,(cdar pairs))))))
           parts)
       ,(if (null? last-case) '(begin) last-case))))

(defmac deriveEq (derive-class 'Eq '== '(def (== _ _) False) '&&))

;;; Assertions

(assert (list (+ 1 2) 3 4)  "((:) ((+) 1 2) ((:) 3 ((:) 4 [])))")

(assert
  (let= (x (* 2 4)
         y (+ x 4))
    (* x y))
"(let {
    x = ((*) 2 4);
    y = ((+) x 4);
  } in ((*) x y))")

(assert
  (let>>= (myfile (readfilei "test")
           a (myfile 0)
           b (myfile 1))
    (print (tup a b)))
"((>>=) (readfilei \"test\") (\\myfile -> ((>>=) (myfile 0) (\\a -> ((>>=) (myfile 1) (\\b -> (print (a, b))))))))")

(assert
  (def (step v s e)
    (letl (step- (n 0))
      (def next (+ s (* n v)))
      (if- (> next e) (list) (cons next (step- (inc n))))))
"step = (\\v -> (\\s -> (\\e -> (let {
    step' = (\\n -> (let {
    next = ((+) s ((*) n v));
  } in (if' ((>) next e) [] (cons next (step' (inc n))))));
  } in (step' 0)))))")

(assert
  (cond
    ((> a b) (def c (+ a 2)) (* c b))
    ((> a 3) (* a 2))
    (- a b))
"(condList ((:) (((>) a b), (let {
    c = ((+) a 2);
  } in ((*) c b))) ((:) (((>) a 3), ((*) a 2)) [])) ((-) a b))")

(assert (case x (1 "A") (2 "B") (3 "C") "D")
  "(caseList x ((:) (1, \"A\") ((:) (2, \"B\") ((:) (3, \"C\") []))) \"D\")")

