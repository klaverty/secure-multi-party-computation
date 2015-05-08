(load "p0utils.scm")

#| Kathleen Laverty -- 6.945 |#

#| Problem 1: Modular Arithmetic |#

#| Test Cases:
(modulo 13 8)
;Value: 5

(remainder 13 8)
;Value: 5

(modulo -13 8)
;Value: 3

(remainder -13 8)
;Value: -5

(modulo -13 -8)
;Value: -5

(remainder -13 -8)
;Value: -5
|#

#| What is the difference between remainder and modulo?
     Remainder computes the actual remainder (i.e. solves the equation n/d = q + r/d)
     while modulo ensures that the remainder is in the proper range by addding or subtracting
     multiples of the modulus.
   Which one is the best choice for implementing modular arithmetic as described above?
     Modulo is the best choice for this case.
|#

 
(define +mod
  (lambda (a b n)
    (modulo (+ a b) n)))
;Value: +mod


(define -mod
  (lambda (a b n)
    (modulo (- a b) n)))
;Value: -mod


(define *mod
  (lambda (a b n)
    (modulo (* a b) n)))
;Value: *mod

(+mod 7 5 8)
;Value: 4

(+mod 10 10 3)
;Value: 2

(-mod 5 12 2)
;Value: 1

(*mod 6 6 9)
;Value: 0

(+mod 99 99 100)
;Value: 98

(*mod 50 -3 100)
;Value: 50


(define modular
  (lambda (modulus op)
    (lambda (a1 a2)
      (modulo (op a1 a2) modulus))))
;Value: modular

((modular 17 +) 13 11)
;Value: 7

((modular 17 -) 13 11)
;Value: 2

((modular 17 *) 13 11)
;Value: 7

#| Problem 2: Raising a Number to a Power |#


(define (slow-exptmod n)
  (let ((*mod (modular n *)))
    (define (em a b)
      (if (= b 0)
	  1
	  (*mod a (em a (- b 1)))))
    em))
;Value: slow-exptmod

#| What is the order of growth in time of slow-exptmod?
     O(b)
   What is the order of growth in space?
     O(b)
   Does slow-exptmod use an iterative algorithm or a recursive algorithm?
     recursive
|#

(define (exptmod p)
  (let ((mod* (modular p *)))
    (define (square x)
      (mod* x x))
    (define (even? n)
      (= (remainder n 2) 0))
    (define (em base exponent)
      (cond ((= exponent 0) 1)
	    ((even? exponent) (square (em base (/ exponent 2))))
	    (else (mod* base (em base (- exponent 1))))))
    em))
;Value: exptmod

((exptmod 10) 2 0)
;Value: 1

((exptmod 10) 2 3)
;Value: 8

((exptmod 10) 3 4)
;Value: 1

((exptmod 100) 2 15)
;Value: 68

((exptmod 100) -5 3)
;Value: 75


#| What is the order of growth in time of your implementation of exptmod?
     log(exponent)
   What is the order of growth in space?
     log(exponent)
   Does exptmod use an iterative algorithm or a recursive algorithm?
     recursive
|#

#| Problem 3: Large Random Numbers |#

(define (random-k-digit-number k)
  (define (iter counter num)
    (if (> counter 0)
	(iter (- counter 1) (+ num (* (expt 10 (- counter 1)) (random 10))))
	num))
  (iter k 0))
;Value: random-k-digit-number

(random-k-digit-number 1)
;Value: 5

(random-k-digit-number 3)
;Value: 611

(random-k-digit-number 3)
;Value: 568

(random-k-digit-number 50)
;Value: 10932957896600558543689882014362391193799686994005


(define (count-digits n)
  (define (iter n digits)
    (if (> n 0)
	(iter (quotient n 10) (+ 1 digits))
	digits)
    )
  (iter n 0))
;Value: count-digits

(count-digits 3)
;Value: 1

(count-digits 2007)
;Value: 4

(count-digits 123456789)
;Value: 9


(define (big-random n)
  (let ((random-num (random-k-digit-number (count-digits n))))
    (if (>= random-num n)
	(big-random n)
	random-num)
    )
)
;Value: big-random

(big-random 100)
;Value: 72

(big-random 100)
;Value: 84

(big-random 1)
;Value: 0

(big-random 1)
;Value: 0

(big-random (expt 10 40))
;Value: 4429275457915439354685636699064376698621

#| Problem 4: Prime Numbers |#

(define (slow-prime? n)
  (define (test-factors n k)
    (cond ((>= k n) #t)
	  ((= (remainder n k) 0) #f)
	  (else (test-factors n (+ k 1)))))
  (if (< n 2)
      #f
      (test-factors n 2)))
;Value: slow-prime?

#| What is the order of growth in time of slow-prime?
     O(n*(time to compute remainder))
   What is its order of growth in space?
     O(n)
   Does slow-prime use an iterative algorithm or recursive algorithm?
     iterative algorithm
|#

#| If we only checked factors less than or equal to the sqrt(n),
   the order of growth in time would become O(sqrt(n)*(time to compute remainder))
|#

#| If we only checked odd factors (and 2), we could cut the worst-case original
   number of operations in half. However, 1/2 is just a constant so it would still be
   O(n*(time to compute remainder))
|#

#| Testing Fermat's Little Theorem

((exptmod 7) 2 7)
;Value: 2

((exptmod 7) 3 7)
;Value: 3

((exptmod 7) 4 7)
;Value: 4

((exptmod 17) 45 17)
;Value: 11

|#

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
;Value: prime?

(prime? 2)
;Value: #t

(prime? 4)
;Value: #f

(prime? 1)
;Value: #f

(prime? 0)
;Value: #f

(prime? 200)
;Value: #f

(prime? 199)
;Value: #t

#| What is the order of growth in time of your implementation of prime?
      O(log(p))
   What is its order of growth in space?
      O(log(p))
   Does prime? use an iterative algorithm or a recursive algorithm?
      iterative algorithm
|#

#| Problem 5: Random Primes |#

(define random-k-digit-prime
  (lambda (k)
    (let ((p (random-k-digit-number k)))
      (if (prime? p)
          p
          (random-k-digit-prime k))
      )
    )
  )
;Value: random-k-digit-prime

#| In what ways can your random-prime procedure fail?
      The procedure can fail by generating a number that is
      not prime. The probabalistic primality test we wrote
      does not guarantee the output will always be prime.
|#

(random-k-digit-prime 1)
;Value: 3

(random-k-digit-prime 2)
;Value: 97

(random-k-digit-prime 10)
;Value: 468752831

(count-digits (random-k-digit-prime 100))
;Value: 100

(count-digits (random-k-digit-prime 100))
;Value: 100

#| Problem 6: Multiplicative Inverses |#

(define ax+by=1
  (lambda (a b)
    (let ((q (quotient a b))
	  (r (remainder a b)))
      (if (= r 1)
	  (list 1 (* q -1))
	  (let ((s (ax+by=1 b r)))
	    (list (second s) (- (first s) (* q (second s)))))))))
;Value: ax+by=1

(ax+by=1 17 13)
;Value 16: (-3 4)

(ax+by=1 7 3)
;Value 17: (1 -2)

(ax+by=1 10 27)
;Value 18: (-8 3)

(define (inversemod n)
  (lambda (e)
    (if (= (gcd e n) 1)
	(modulo (first (ax+by=1 e n)) n)
	'error)))
;Value: inversemod

((inversemod 11) 5)
;Value: 9

((inversemod 11) 9)
;Value: 5

((inversemod 11) 7)
;Value: 8

((inversemod 12) 5)
;Value: 5

((inversemod 12) 8)
;Value: error

((inversemod 101) (random-k-digit-prime 2))
;Value: 53

#| Problem 7: The ElGamal Public-Key Cryptosystem |#

(load "p0utils")

(define (eg-send-message message receiver)
  (let* ((pk (eg-receiver-public-key receiver))
	(dh-system (car pk))
	(a^s (cdr pk))
	(m (string->integer message))
	(k (dh-system-size dh-system))
	(a (dh-system-primitive-root dh-system))
	(p (dh-system-prime dh-system))
	(t (random-k-digit-number k))
	(x ((exptmod p) a t))
	(y ((modular p *) m ((exptmod p) a^s t)))
	(c (eg-make-ciphertext x y))
	(receiver-decryption-procedure
	  (eg-receiver-decryption-procedure receiver)))
  (receiver-decryption-procedure c)))

;; Test cases:
;;
;; (eg-send-message "Hi there." Alyssa) 
;; ;Value 13: "Hi there."
;;
;; (eg-send-message "How are you?" Alyssa)  
;; ;Value 14: "How are you?"

#| Problem 8: “Man In The Middle” Attack |#
;; Eve can make trouble by tampering with the  
;; messages Ben sends to Alyssa.
;;
;; The following code demonstrates how Eve can add 1 
;; to the first part of the ciphertext and as a result,
;; the message becomes completely scrambled when decoded.

(define (Eve receiver)
  (let ((receiver-public-key
	 (eg-receiver-public-key receiver))
	(receiver-decryption-procedure
	 (eg-receiver-decryption-procedure receiver)))
    (let ((my-spying-procedure
	   (lambda (ciphertext)
	     (write ciphertext)
	     (newline)
	     (receiver-decryption-procedure 
	       (eg-make-ciphertext (+ (car ciphertext) 1) (cdr ciphertext))))))
      (eg-make-receiver receiver-public-key
			my-spying-procedure))))

;; Test cases:
;; (define dh-system (public-dh-system 100))
;; ;Value: dh-system
;;
;; (define Alyssa (eg-receiver dh-system))
;; ;Value: alyssa
;;
;; (define Alyssa (Eve Alyssa))
;; ;Value: alyssa
;;
;; (eg-send-message "Hi there." Alyssa)
;; (747033880687694035254634415205093551175620757235060096611559299766318938856773824117957978457778193 . 1510473550949041048641206815111184717794638905095649211889689167760418765613417446145575187285021707)
;; ;Value 13: "]g?Qz??.<u033e\236?\027l O\023@޳\a??p$?\233%?\220\001"
