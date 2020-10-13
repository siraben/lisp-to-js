;; A fancier program that computes the first 20 prime numbers.

(define (not x) (if x #f #t))
(define (divisiblep a b) (= 0 (modulo b a)))
(define head car)
(define (tail l) ((cdr l)))
(define (take n s)
  (if (= n 0)
      '()
      (cons (head s) (take (- n 1) (tail s)))))
(define (integers_from n)
  (cons n (lambda () (integers_from (+ n 1)))))
(define (sieve s)
  (cons (head s)
        (lambda ()
          (sieve (filter_stream
                  (lambda (x)
                    (not (divisiblep (head s) x))) (tail s))))))
(define (filter_stream p s)
  (if (p (head s))
      (cons (head s) (lambda () (filter_stream p (tail s))))
      (filter_stream p (tail s))))

(define primes (sieve (integers_from 2)))

(take 20 primes)
