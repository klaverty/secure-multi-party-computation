(define (random-k-digit-number k)
  (define (iter counter num)
    (if (> counter 0)
      (iter
	(- counter 1)
	(+ num
	   (* (expt
		10
		(- counter 1))
	      (random 10))))
      num))
  (iter k 0))

(define random-k-digit-prime
  (lambda (k)
    (let ((p (random-k-digit-number k)))
      (if (prime? p)
	p
	(random-k-digit-prime k)))))

(define prime-test-iterations 20)

(define prime?
  (lambda (p)
    (define (prime-iter pr count)
      (cond ((= count 0) #t)
	    ((< pr 2) #f)
	    (else
	      (let ((r (big-random pr)))
		(if (= ((exptmod pr) r pr) (modulo r pr))
		  (prime-iter pr (- count 1))
		  #f)))))
    (prime-iter p prime-test-iterations)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              POLYNOMIALS                   ;;;
;;;     which we represent as lists of         ;;;
;;;     coefficients that begin with the       ;;;
;;;     tag 'poly.                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Generate a polynomial
;; of degree 'n' with random nonnegative 
;; coefficients < p except the
;; constant coefficient which is 'c'
;; (meaning p(0) = c)
(define generate-rand-poly
  (lambda (c n p)
    (let lp ((n n)
	     (coeffs '()))
      (if (= n 0)
	(cons 'poly (cons c coeffs))
	(lp (- n 1) (cons (random p) coeffs))))))

;; Evaluate the polynomial 'poly'
;; at the input 'num' (i.e. poly(num))
(define poly-eval
  (lambda (poly num p)
    (let lp ((n 0)
	     (coeffs (cdr poly))
	     (value 0))
      (if (eq? coeffs '())
	(modulo value p)
	(lp (+ n 1)
	    (cdr coeffs)
	    (+ value (* (expt num n) (car coeffs))))))))

;; Check if the elements of a list
;; 'items' are all distinct (from sicp)
(define (distinct? items)
  (cond ((null? items) true)
	((null? (cdr items)) true)
	((member (car items) (cdr items)) false)
	(else (distinct? (cdr items)))))

;; Convert a polynomial 'poly'
;; from coefficients into point-value
;; notation meaning a list of n+1
;; pairs (a, b) where p(a) = b
(define point-value-poly
  (lambda (poly p)
    (share-t poly (length poly) p)))

;; Divide a polynomial into t
;; shares of information.
(define share-t
  (lambda (poly t p)
    (let lp ((points '())
	     (n t))
      (if (= n 0)
	(if (distinct? points)
	  points
	  (point-value-poly poly p))
	(let* ((a (+ 1 (random (- p 1))))
	       (b (modulo (poly-eval poly a p) p)))
	  (lp (cons (cons a b) points) (- n 1)))))))


;; Convert a polynomial from point-value
;; notation into coefficient notation
(define reconstruct-poly
  (lambda (point-pairs x p)
    ;;modular inverse in Z_p
    (define (mod-inv  p)
      (lambda (a)
      (modulo (expt a (- p 2)) p)))

    ;; Helper function to compute terms of the 
    ;; Lagrange cofficient
    (define multiplicand
      (lambda (x_i)
	(lambda (pair)
	  (let ((x_j (car pair)))
	    (if (= x_i x_j)
	      1
	      (* (- x x_j) ((mod-inv p) (- x_i x_j))))))))

    (let lp ((value 0)
	     (points (list-copy point-pairs)))
      (if (eq? points '())
	(modulo value p)
	(let* ((x_i (caar points))
	       (y_i (cdar points))
	       (k_i (apply * (map (multiplicand x_i) point-pairs))))
	  (lp (+ value (* y_i k_i)) (cdr points)))))))

;; Adding two polynomials
(define poly-add
  (lambda (poly-a poly-b)
    (define (list-sum a b)
      (if (eq? a '())
	'()
	(cons (+ (car a) (car b))
	      (list-sum (cdr a) (cdr b))
	    )))
    (cons 'poly (list-sum (cdr poly-a) (cdr poly-b)))))

