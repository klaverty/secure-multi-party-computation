;; A simple test

(define num-players 2)

(define multiparty
  (make-multiparty num-players))

(pp "")

(define prime (random-k-digit-prime 2))

(pp "Prime: ")
(pp prime)

(define rand-list (generate-rand-list num-players prime))

(for-each (lambda (player)
	    (let ((r (+ (random 5) 1)))
	      (begin
		(pp "Some player voted")
		(pp r)
		(set-input! player r))))
	  multiparty)

(let lp ((indices (list-copy rand-list))
	 (players (list-copy multiparty)))
  (if (not (eq? indices '()))
    (begin
      (set-id! (car players) (car indices))
      (lp (cdr indices) (cdr players)))))

(pp "sharing secret")
(secret-share-inputs multiparty prime)
(pp "secrets shared")

(distributed-multiply multiparty rand-list 'out prime)
