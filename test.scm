;; A simple test

(define multiparty
  (make-multiparty 10))

(pp "")

(for-each (lambda (player)
	    (let ((r (random 2)))
	      (begin
		(pp "Some player voted")
		(pp r)
		(set-player-info! player 'input r))))
	  multiparty)

(distributed-sum multiparty)
