(load "prime-utils")

;; Check if the elements of a list
;; 'items' are all distinct (from sicp)
(define (distinct? items)
  (cond ((null? items) true)
	((null? (cdr items)) true)
	((member (car items) (cdr items)) false)
	(else (distinct? (cdr items)))))

(define (generate-rand-list n p)
  (let lp ((rand-list '())
	   (n n))
    (if (= n 0)
      (if (distinct? rand-list)
	rand-list
	(generate-rand-list n p))
      (lp
	(cons
	  (random p)
	  rand-list)
	(- n 1)))))


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

;;Add the point representation of two polynomial
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
		 (cons (cons eval-point-a
			     (+ (cdr head-a) (cdr head-b)))
		       (lp (cdr points-a) (cdr points-b)))
		 (error "Incompatible representation!")))))))

