
;; put your macros here...

(defmac (defdo formal . rest)
  `(def ,formal ,(if (null? (cdr rest)) (car rest) (cons '>> rest))))

(defmac (letdo decls  . rest)
  `(let>>= ,decls ,(if (null? (cdr rest)) (car rest) (cons '>> rest))))

