;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;           MODULAR ARITHMETIC               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define +mod
  (lambda (a b n)
    (modulo (+ a b) n)))

(define -mod
  (lambda (a b n)
    (modulo (- a b) n)))

(define *mod
  (lambda (a b n)
    (modulo (* a b) n)))

(define modular
  (lambda (modulus op)
    (lambda (a1 a2)
      (modulo (op a1 a2) modulus))))

(define (exptmod p)
  (let ((mod* (modular p *)))
    (define (square x)
      (mod* x x))
    (define (even? n)
      (= (remainder n 2) 0))
    (define (em base exponent)
      (cond ((= exponent 0) 1)
	    ((even? exponent)
	     (square (em base (/ exponent 2))))
	    (else
	      (mod* base
		    (em base (- exponent 1))))))
    em))

(define ax+by=1
  (lambda (a b)
    (let ((q (quotient a b))
	  (r (remainder a b)))
      (if (= r 1)
	  (list 1 (* q -1))
	  (let ((s (ax+by=1 b r)))
	    (list
	      (second s)
	      (- (first s)
		 (* q (second s)))))))))

(define (inversemod n)
  (lambda (e)
    (if (= (gcd e n) 1)
	(modulo (first (ax+by=1 e n)) n)
	'error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             RANDOMS & PRIMES               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (random-k-digit-number k)
  (define (iter counter num)
    (if (> counter 0)
	(iter
	  (- counter 1)
	  (+ num
	     (* (expt 10 (- counter 1))
		(random 10))))
	num))
  (iter k 0))

(define (count-digits n)
  (define (iter n digits)
    (if (> n 0)
	(iter (quotient n 10) (+ 1 digits))
	digits)
    )
  (iter n 0))

(define (big-random n)
  (let ((random-num
	  (random-k-digit-number
	    (count-digits n))))
    (if (>= random-num n)
	(big-random n)
	random-num)))

(define prime-test-iterations 20)

(define prime?
  (lambda (p)
    (define (prime-iter pr count)
      (cond ((= count 0) #t)
	    ((< pr 2) #f)
	    (else
	     (let ((r (big-random pr)))
	       (if (= ((exptmod pr) r pr)
		      (modulo r pr))
		   (prime-iter pr (- count 1))
		   #f)))))
    (prime-iter p prime-test-iterations)))

(define random-k-digit-prime
  (lambda (k)
    (let ((p (random-k-digit-number k)))
      (if (prime? p)
          p
          (random-k-digit-prime k)))))

;; Check if the elements of a list
;; 'items' are all distinct (from sicp)
(define (distinct? items)
  (cond ((null? items) true)
	((null? (cdr items)) true)
	((member (car items) (cdr items)) false)
	(else (distinct? (cdr items)))))

;; Generate a list of n distinct random
;; numbers in the range 0 to p-1.
(define (generate-rand-list n p)
  (let lp ((rand-list '())
	   (k n))
    (if (= k 0)
      (if (distinct? rand-list)
	rand-list
	(generate-rand-list n p))
      (lp
	(cons
	  (random p)
	  rand-list)
	(- k 1)))))


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
	(lp (- n 1)
	    (cons (random p) coeffs))))))

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
	    (+ value
	       (* (expt num n)
		  (car coeffs))))))))

;; Convert a polynomial 'poly'
;; from coefficients into point-value
;; notation meaning a list of n+1
;; pairs (a, b) where p(a) = b
(define point-value-poly
  (lambda (poly eval-points p)
    (share-t poly eval-points p)))

;; Divide a polynomial into t
;; shares of information.
(define share-t
  (lambda (poly eval-points p)
    (let lp ((points '())
	     (eval-points eval-points))
      (if (eq? eval-points '())
	points
	(let* ((a (car eval-points))
	       (b (modulo (poly-eval poly a p) p)))
	  (lp (cons b points) (cdr eval-points)))))))


;; Convert a polynomial from point-value
;; notation into coefficient notation
(define reconstruct-poly
  (lambda (point-pairs x p)
    ;;modular inverse in Z_p
    (define (mod-inv p)
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
	      (* (- x x_j)
		 ((mod-inv p)
		  (- x_i x_j))))))))

    (let lp ((value 0)
	     (points (list-copy point-pairs)))
      (if (eq? points '())
	(modulo value p)
	(let* ((x_i (caar points))
	       (y_i (cdar points))
	       (k_i
		 (apply
		   *
		   (map
		     (multiplicand x_i)
		     point-pairs))))
	  (lp (+ value (* y_i k_i))
	      (cdr points)))))))

;; Adding two polynomials
(define poly-add
  (lambda (poly-a poly-b)
    (define (list-sum a b)
      (if (eq? a '())
	'()
	(cons (+ (car a) (car b))
	      (list-sum
		(cdr a) (cdr b)))))
    (cons 'poly (list-sum
		  (cdr poly-a) (cdr poly-b)))))

;;Add the point representation of two polynomials
(define points-add 
  (lambda (points-a points-b)
    (let lp ((points-a points-a)
	     (points-b points-b))
	     (if (eq? points-a '())
	       '()
	       (let* ((head-a (car points-a))
		      (head-b (car points-b))
		      (eval-point-a (car head-a))
		      (eval-point-b (car head-b)))
		 (if (eq? eval-point-a eval-point-b)
		 (cons
		   (cons
		     eval-point-a
		     (+ (cdr head-a) (cdr head-b)))
		   (lp (cdr points-a) (cdr points-b)))
		 (error "Incompatible representation!")))))))

