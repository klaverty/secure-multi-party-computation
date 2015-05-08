
;; Generate a random polynomial
;; of degree n which has a constant
;; coefficient of c
(define generate-rand-poly
  (lambda (c n p)
    (let lp ((n n)
	     (coeffs '())
      (if (= n 0)
	(cons 'poly (cons c coeffs))
	(lp (- n 1) (cons (random p) coeffs)))))))
