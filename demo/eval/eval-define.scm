;; A meta-circular interpreter in Scheme
;; Adapted from http://paulgraham.com/rootsoflisp.html

;; This variant tests define.  Because of the way we desugar define
;; into let and set!, mutual recursion is also achieved easily.  

(define eq eqv?)
(define atom (lambda (x) (if (null? x) #t (if (symbol? x) #t #f))))
(define not (lambda (x) (if x #f #t)))
(define and (lambda (x y) (if x (if y #t #f) #f)))
(define cadr (lambda (x) (car (cdr x))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define caar (lambda (x) (car (car x))))
(define caddar (lambda (x) (car (cdr (cdr (car x))))))
(define cadar (lambda (x) (car (cdr (car x)))))

(define (assoc x y)
  (if (eq (caar y) x)
      (cadar y)
      (assoc x (cdr y))))

(define (pair x y)
  (if (and (null? x) (null? y))
      '()
      (if (and (not (atom x))
               (not (atom y)))
          (cons (list (car x) (car y))
                (pair (cdr x) (cdr y))))))

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (evcon c a)
  (if (evalf (caar c) a)
      (evalf (cadar c) a)
      (evcon (cdr c) a)))

(define (evlis m a)
  (if (null? m)
      '()
      (cons (evalf (car m) a)
            (evlis (cdr m) a))))

(define (evalf e a)
  (cond ((atom e)
         (assoc e a))
        ((atom (car e))
         (cond ((eq (car e) 'quote) (cadr e))
               ((eq (car e) 'atom) (atom (evalf (cadr e) a)))
               ((eq (car e) 'eq) (eq (evalf (cadr e) a)
                                     (evalf (caddr e) a)))
               ((eq (car e) 'car) (car (evalf (cadr e) a)))
               ((eq (car e) 'cdr)
                (cdr (evalf (cadr e) a)))
               ((eq (car e) 'cons)
                (cons (evalf (cadr e) a)
                      (evalf (caddr e) a)))
               ((eq (car e) 'cond)
                (evcon (cdr e) a))
               ('t
                (evalf (cons (assoc (car e) a)
                             (cdr e))
                      a))))
        ((eq (caar e) 'label)
         (evalf (cons (caddar e) (cdr e))
               (cons (list (cadar e) (car e)) a)))
        ((eq (caar e) 'lambda)
         (evalf (caddar e)
               (append (pair (cadar e)
                             (evlis (cdr e) a))
                       a)))))

(console.log (evalf '((label firstatom
                             (lambda (x)
                               (cond ((atom x) x)
                                     ('t (firstatom (car x))))))
                      y)
                    '((y ((a b) (c d))))))

(console.log (evalf '((label subst
                             (lambda (x y z)
                               (cond ((atom z)
                                      (cond ((eq z y) x)
                                            ('t z)))
                                     ('t (cons (subst x y (car z))
                                               (subst x y (cdr z)))))))
                      'm
                      'b
                      '(a b (a b c) d))
                    '()))
