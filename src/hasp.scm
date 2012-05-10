
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

;;;; This code converts s-expressions (hasp code) into Haskell code.
;;;; The supported constructs can be seen at the bottom of this file.
;;;; "Macros" can also be made using the scheme code:
;;;;
;;;; (defmac (name arg1 arg2) `(expanded ,arg1 ,arg2))
;;;;
;;;; This generates s-expressions (hasp code) to be converted into haskell code.
;;;;
;;;; To compile a file from hasp code to haskell code, do:
;;;;
;;;; (load "hasp.scm")
;;;; (load "my-macros.scm") ; Optional - any "macros" used by "my-exprs.hasp"
;;;; (acc "my-exprs.hasp" "my-code.hs")
;;;;
;;;; my-code.hs will be created and overwritten with the haskell code.
;;;;
;;;; This code is very untested, but should work more or less okay.
;;;; See the bottom of this file for the assertion checks that the code passes.

;;; Numbers

(define (dec n) (- n 1))
(define (inc n) (+ n 1))

;;; Lists

(define (butlast l)
  (if (or (null? l) (null? (cdr l))) '()
    (cons (car l) (butlast (cdr l)))))

(define (last l)
  (if (null? (cdr l)) (car l) (last (cdr l))))

;;; Assoc lists

(define (assoc-set lst key val)
  (if (null? lst)
    (list (cons key val))
    (let ((next (car lst)))
      (if (equal? key (car next))
        (cons (cons key val) (cdr lst))
        (cons next (assoc-set (cdr lst) key val))))))

(define (assoc-ref-nfv d n not-found-value)
  (if (null? d)
    not-found-value
    (let ((nex (car d)))
      (if (eq? (car nex) n)
        (cdr nex)
        (assoc-ref-nfv (cdr d) n not-found-value)))))

;;; Strings

(define (merge-strings a) (apply string-append a))

(define (ends-in-newline? str)
  (and (not (equal? str ""))
    (char=? (string-ref str (dec (string-length str))) #\newline)))

(define (string-find x y)
  (let ((xlen (string-length x)) (ylen (string-length y)))
    (let loop ((curx 0) (cury 0))
      (if (= cury ylen)
        (- curx ylen)
        (if (= curx xlen)
          #f
          (if (char=? (string-ref x curx) (string-ref y cury))
            (loop (inc curx) (inc cury))
            (loop (inc curx) 0)))))))

(define (string-replace s a b)
  (let ((alen (string-length a)))
    (let ((f (string-find s a)))
      (if f
        (string-append (substring s 0 f) b
          (string-replace (substring s (+ f alen) (string-length s)) a b))
        s))))

(define (string-repeat str n)
  (if (zero? n) "" (string-append str (string-repeat str (dec n)))))

(define (implode glue l)
  (let loop ((acc "") (la l))
    (if (null? la) acc
      (loop
        (string-append acc
          (if (null? (cdr la)) (car la) (string-append (car la) glue)))
        (cdr la)))))

(define (backslash a d)
  (string-replace
    (string-replace (string-replace a "\\" "\\\\") d (string-append "\\" d))
    "\n"
    "\\n"))

(define (to-str a)
  (cond
    ((symbol? a) (symbol->string a))
    ((string? a) a)
    ((number? a)
      (let ((rv (number->string a)))
        (if (char=? (string-ref rv 0) #\.) (string-append "0"rv) rv)))
    ((list? a) (implode " " (map to-str a)))
    ((char? a) (string a))
    (else a)))

;;; Lambdas

(define (partial fun . early) (lambda late (apply fun (append early late))))

;;; Domain specific code

(define forms (make-table))

(define (hasp-to-hs-ext x)
  (let* ((len (string-length x))
         (ext ".hasp")
         (extlen (string-length ext))
         (end-part (substring x (- len extlen) len)))
    (string-append
      (if (equal? end-part ext)
        (substring x 0 (- len extlen))
        x)
      ".hs")))

(define (hs-undash-symbol a)
  (let* ((rv (to-str a))
         (rvlen (string-length rv)))
    (let loop ((i 0))
      (if (= i rvlen)
        rv
        (if (char=? (string-ref rv i) #\-)
          (loop (inc i))
          (let loop2 ((j (dec rvlen)))
            (if (char=? (string-ref rv j) #\-)
              (loop2 (dec j))
              (string-append (substring rv 0 (inc j))
                (string-repeat "'" (- rvlen (inc j)))))))))))

(define (hs-str a)
  (cond
    ((symbol? a)
      (let ((v (table-ref forms a #f)))
        (if (and v (eq? (car v) 'infix))
          (string-append "("(hs-undash-symbol (cdr v))")")
          (if (eq? a 'Nil)
            "[]"
            (hs-undash-symbol a)))))
    ((string? a) (string-append "\"" (backslash a "\"") "\""))
    ((and (number? a) (< a 0)) (string-append "("(to-str a)")"))
    ((list? a) (string-append "("(implode " " (map hs-str a))")"))
    ((char? a) (string-append "'"(backslash (to-str a) "'")"'"))
    (else (to-str a))))

(define (data-str a)
  (cond
    ((symbol? a) (hs-str (to-str a)))
    ((list? a) (string-append "["(implode ", " (map data-str a))"]"))
    (else (hs-str a))))

(define ident (partial string-repeat "  "))

(define (splat x)
  (if (and (list? x) (not (table-ref forms (car x) #f)))
    (implode " " (map top-compile x))
    (top-compile x)))

(define (splat-items a)          (if-list a (map splat a)))
(define (back-leveled-split s a) (if-list a (implode s (splat-items a))))
(define (items-or-sym s a)       (if-list a (implode s (map top-compile a))))

(define space-split (partial items-or-sym " "))
(define arrow-split (partial items-or-sym " -> "))
(define comma-split (partial items-or-sym ", "))

(define union-splat (partial back-leveled-split " | "))
(define arrow-splat (partial back-leveled-split " -> "))
(define comma-splat (partial back-leveled-split ", "))

(define (map-merge fn forms) (merge-strings (map fn forms)))
(define (compile-merge i forms) (map-merge (partial compile i) forms))

(define (statement-merge i forms)
  (map-merge
    (lambda (x)
      (let ((rv (statement i x)))
        (if (ends-in-newline? rv)
          rv
          (string-append rv "\n"))))
    forms))

(define (newline-statement-merge i forms)
  (map-merge (lambda (x) (string-append "\n" (statement i x))) forms))

(define (procedure-call i form)
  (string-append (ident i) "("(space-split form)")"))

(define (where-form i a)
  (let ((rv (if (null? a) ""
              (string-append (statement (inc i) (car a))
                (newline-statement-merge (inc i) (cdr a))))))
    (string-append "where {\n"
      (if (ends-in-newline? rv) rv (string-append rv "\n"))
      (ident i)"}")))

(define pseudo-sym-count 0)

(define (pseudo-sym s . prefix)
  (let ((v (string->symbol
             (string-append s"_gensym_"
               (if (null? prefix) "" (string-append (to-str (car prefix)) "_"))
               (number->string pseudo-sym-count)))))
    (set! pseudo-sym-count (inc pseudo-sym-count))
    v))

(define gens (partial pseudo-sym "s"))
(define gent (partial pseudo-sym "T"))

;;; The "compiler"

(define (hook-form? form)
  (and (symbol? (car form))
    (char=? (string-ref (symbol->string (car form)) 0) #\:)))

(define (macroexpand form)
  (if (and (list? form) (not (null? form)))
    (let ((v (table-ref forms (car form) #f)))
      (if (and v (eq? (car v) 'macro))
        (macroexpand (apply (cdr v) (cdr form)))
        form))
    form))

(define (compile i expr)
  (let ((form (macroexpand expr)))
    (if (and (list? form) (not (null? form)))
      (let ((v (table-ref forms (car form) #f)))
        (if (and v (not (eq? (car v) 'infix)))
          (apply (cdr v) i (cdr form))
          (if (and (or (not v) (not (eq? (car v) 'infix))) (hook-form? form))
            (implode " " (map top-compile form))
            (procedure-call i
              (if (and v (eq? (car v) 'infix) (> (length form) 3))
                (list (car form) (cadr form) (cons (car form) (cddr form)))
                form)))))
      (string-append (ident i) (hs-str form)))))

(define (top-compile a) (compile 0 a))
(define (compile-line i form) (string-append (compile i form) "\n"))

(define (compile-colon i form)
  (string-append (compile i form) (if (eq? (car form) '::) ";" ";\n")))

(define (statement i expr)
  (let ((form (macroexpand expr)))
    (if (and (list? form) (not (null? form)))
      (let ((v (table-ref forms (car form) #f)))
        (if (and v (eq? (car v) 'general))
          (compile-colon i form)
          (compile i form)))
      (compile-line i form))))

;;; The REPL top level

(define (acc-output-expr flattenp o expr)
  (display
    (string-append
      (let ((rv (statement 0 expr)))
        (if flattenp (string-replace rv "\n" " ") rv))
      "\n")
    o)
  (force-output o))

(define acc-statelist (list))
(define acc-declist (list))
(define acc-deflist (list))

(define (acc-build-decls declist deflist)
  (if (null? declist)
    (if (null? deflist)
      '()
      (cons (cdar deflist) (acc-build-decls declist (cdr deflist))))
    (cons (cdar declist) (acc-build-decls (cdr declist) deflist))))

(define (acc-update-predefs predef expr)
  (let ((present
          (let loop ((r predef))
            (if (null? r) #f
              (if (equal? (cadar r) (cadr expr)) #t (loop (cdr r)))))))
    (if present
      (let loop ((r predef))
        (if (null? r) '()
          (if (equal? (cadar r) (cadr expr))
            (cons expr (cdr r))
            (cons (car r) (loop (cdr r))))))
      (cons expr predef))))

(define (acc-update-declaration-file expr)
  (let ((refname
          (if (and (list? (cadr expr)) (not (eq? (car expr) 'instance)))
            (caadr expr)
            (cadr expr))))
    (set! acc-statelist
      (if (or (eq? (car expr) 'import) (eq? (car expr) 'foreign))
        (cons (cons #f expr) acc-statelist)
        (assoc-set acc-statelist refname expr))))
  (let ((f (open-output-file "hasp-repl.hasp")))
    (map
      (lambda (x)
        (newline f)
        (write (cdr x) f)
        (newline f))
      acc-statelist)
    (display "\n" f)
    (close-output-port f)
    (acc "hasp-repl.hasp" "hasp-repl.hs")))

(define (acc-file-statement? expr)
  (let ((r (car expr)))
    (let loop ((l '(type data newtype class instance module import foreign)))
      (if (null? l) #f (if (eq? r (car l)) #t (loop (cdr l)))))))

(define (acc-toplevel-filter flattenp o r)
  (let ((expr (macroexpand r)))
    (if (and (list? expr) (not (null? expr)))
      (if (eq? (car expr) 'begin)
        (map (partial acc-toplevel-filter flattenp o) (cdr expr))
        (if (acc-file-statement? expr)
          (begin
            (acc-update-declaration-file expr)
            (acc-output-expr flattenp o '(:load "hasp-repl.hs")))
          (if (eq? (car expr) '::)
            (begin
              (set! acc-declist (assoc-set acc-declist (cadr expr) expr))
              (acc-output-expr flattenp o '()))
            (if (eq? (car expr) 'def)
              (begin
                (set! acc-deflist
                  (assoc-set acc-deflist
                    (if (list? (cadr expr)) (caadr expr) (cadr expr)) expr))
                (acc-output-expr flattenp o '()))
              (acc-output-expr flattenp o
                (if (hook-form? expr)
                  expr
                  `(let ,(acc-build-decls acc-declist acc-deflist) ,expr)))))))
      (acc-output-expr flattenp o
        `(let ,(acc-build-decls acc-declist acc-deflist) ,expr)))))

;;; In goes Hasp, out goes Haskell

(define (acc-read fin fout reader flattenp)
  (let ((f (if (string? fin)  (open-input-file fin) fin))
        (o (if (string? fout) (open-output-file fout) fout)))
    (if flattenp #f (newline o))
    (let loop ()
      (let ((r (reader f)))
        (if (eof-object? r)
          (begin
            (if flattenp #f (newline o))
            (if (string? fin) (close-input-port f) #f)
            (if (string? fout) (close-output-port o) #f))
          (begin
            ((if flattenp acc-toplevel-filter acc-output-expr) flattenp o r)
            (loop)))))))

(define (acc-flat fin fout reader) (acc-read fin fout reader #t))
(define (acc fin fout)             (acc-read fin fout read #f))
(define (acc-sugar fin fout)       (acc-read fin fout sugar-read-file #f))

;;; The basic statements

(defstatement i (begin . forms) (statement-merge i forms))

(defstatement i (let declarations . forms)
  (if (and (null? declarations) (null? (butlast forms)))
    (top-compile (last forms))
    (string-append
      "(let {\n"
      (statement-merge (+ i 2) (append declarations (butlast forms)))
      (ident (inc i))"} in "(top-compile (last forms))")")))

(defstatement i (case-of x . forms)
  "(case "(top-compile x)" of {\n"
  (implode "\n"
    (map
      (lambda (form)
        (string-append (ident (inc i)) (top-compile (car form))" -> "
          (top-compile `(let () ,@(cdr form)))";"))
      forms))"})")

(defstatement i (fn formals . forms)
  (if (null? formals)
    (compile i `(let ,(butlast forms) ,(last forms)))
    (string-append
      "(\\"(top-compile (if (list? formals) (car formals) formals))" -> "
        (compile i `(fn ,(if (list? formals) (cdr formals) '()) ,@forms))")")))

(defstatement i (type name typex)
  (ident i)"type "(splat name)" = "(splat typex)";\n")

(defstatement i (data name typex)
  (ident i)"data "(splat name)
  (if (null? typex) "" (string-append " = "(splat typex)))";\n")

(defstatement i (gadt name . signatures)
  (ident i)"data "(splat name)" "(where-form i signatures)";\n")

(defstatement i (newtype name typex)
  (ident i)"newtype "(splat name)" = "(splat typex)";\n")

(defstatement i (class typex . declarations)
  (ident i)"class "(splat typex)
  (if (null? declarations) ""
    (string-append " "(where-form i declarations)))";\n")

(defstatement i (instance typex . statements)
  (ident i)"instance "(splat typex)
  (if (null? statements) ""
    (string-append " "(where-form i statements)))";\n")

(defstatement i (module name exporting . forms)
  (ident i)"module "(compile i name)" "
  (if (null? exporting) "" (string-append "("(comma-splat exporting)") "))
  (where-form i forms))

(defstatement i (import module . names)
  (ident i)"import "
  (if (and (list? module) (or (null? (cdr module)) (list? (cdr module))))
    (string-append "qualified "(top-compile (car module)))
    (top-compile module))
  ""
  (if (and (list? module) (not (null? (cdr module))))
    (string-append " as "(top-compile (if-list (cdr module) (caadr module))))
    "")
  (cond
    ((null? names) "")
    ((null? (car names)) (string-append " "(top-compile (car names))))
    ((list? (car names))
      (string-append " hiding ("(comma-split (car names))")"))
    (else (string-append " ("(comma-split names)")")))
  ";\n")

(defstatement i (tup . items)    "("(comma-split items)")")

(defstatement i (forall types typex)
  "(forall "(space-split types)". "(splat typex)")")

(defstatement i (=> context typex)
  (if (null? context) "" (string-append "("(comma-splat context)") => "))
  (splat typex))

(defstatement i (union . typexes) (union-splat typexes))

(defstatement i (strict typex) "!"(compile i typex))

(defstatement i (:hs . codes) (merge-strings (map to-str codes)))

(defstatement i (:scm . forms)
  "("
  (implode ", "
    (map
      (lambda (x)
        (let ((rv (eval x (interaction-environment))))
          (if (equal? rv (cond (#f #f)))
            "()"
            (data-str rv))))
      forms))
  ")")

(defstatement i (:hload . filenames)
  ":load "
  (implode " "
    (map
      (lambda (x)
        (let ((name (hasp-to-hs-ext x))) (acc-sugar x name) (hs-str name)))
      filenames)))

(defstatement i (-- . text)
  (ident i)"-- "
  (implode " " (map (lambda (t) (string-replace (to-str t) "\n" " ")) text)))

(defstatement i (~~ . text)
  (ident i)"{- "(implode " " (map to-str text))"\n"(ident i)"-}")

(defstatement i (foreign . info) "foreign "(space-split info)";\n")

;;; generals - these are either statements or expressions
;;; depending on the context.
;;; Basically, they can occur inside a record or foreign form.

(defgeneral i (def formals . forms)
  (if (pair? formals)
    (compile i `(def ,(car formals) (fn ,(cdr formals) ,@forms)))
    (string-append
      (ident i) (top-compile formals)" = "
      (compile i `(let ,(butlast forms) ,(last forms))))))

(defgeneral i (:: object typex)
  (ident i) (splat object)" :: "(splat typex))

;;; The basic infix expressions

(definfix -> : !! $ $! && * ** + ++ - / /= < <= == =<< > >= >> >>= ^ ^^ :+ ! (o ".") (or- "||"))

;;; Assertions - we want some good tests to ensure the code is rock solid.

(assert (tup (+ 1 2) 3 4)   "(((+) 1 2), 3, 4)")
(assert (: (+ 1 2) 2)       "((:) ((+) 1 2) 2)")
(assert (+ (+ 1 2) 3)       "((+) ((+) 1 2) 3)")
(assert (fn (a b) (+ a b))  "(\\a -> (\\b -> ((+) a b)))")
(assert (strict (STList a)) "!(STList a)")
(assert (!! lst 10)         "((!!) lst 10)")
(assert (-- "this is foo")  "-- this is foo")
(assert (~~ "this is foo")  "{- this is foo\n-}")
(assert (type TreeOfBool (TreeOf Bool)) "type TreeOfBool = TreeOf Bool;\n")
(assert (type Price (tup Int Int))      "type Price = (Int, Int);\n")
(assert (import (Data.Map (M)))         "import qualified Data.Map as M;\n")

(assert (newtype Natural (MakeNatural Integer))
  "newtype Natural = MakeNatural Integer;\n")

(assert (foreign import ccall safe "prototypes.h" (:: c_function (IO ())))
  "foreign import ccall safe \"prototypes.h\" c_function :: IO ();\n")

(assert
  (def (f x)
    (def (g y) (* 2 y))
    (- (g x) 3))
"f = (\\x -> (let {
    g = (\\y -> ((*) 2 y));
  } in ((-) (g x) 3)))")

(assert (type (ReadS a) (-> String (List (tup a String))))
  "type ReadS a = ((->) String (List (a, String)));\n")

(assert
  (module Main () (import A) (import B)
    (def main (>> (compose A f) (compose B f))))
"module Main where {
  import A;\n
  import B;\n
  main = ((>>) (compose A f) (compose B f));
}")

(assert (import Prelude (lookup filter foldr))
  "import Prelude hiding (lookup, filter, foldr);\n")

(assert
  (class (Num a)
    (:: + (-> a a a))
    (:: negate (-> a a)))
"class Num a where {
  (+) :: ((->) a ((->) a a));
  negate :: ((->) a a);
};\n")

(assert (:: comb (-> (Maybe a) (-> a (Maybe b)) (Maybe b)))
  "comb :: ((->) (Maybe a) ((->) ((->) a (Maybe b)) (Maybe b)))")

(assert (let ((def dolly breedSheep)) (foo dolly))
"(let {
    dolly = breedSheep;
  } in (foo dolly))")

(assert (:: name (-> (Maybe a) (-> Int Integer) Integer))
  "name :: ((->) (Maybe a) ((->) ((->) Int Integer) Integer))")

(assert (data Name (union (A Foo) (B Bar) (C Baz)))
  "data Name = A Foo | B Bar | C Baz;\n")

(assert (instance (Typeable (W a)) (def (typeOf _) (typeOf ())))
"instance Typeable (W a) where {
  typeOf = (\\_ -> (typeOf ()));
};\n")

