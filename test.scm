;;

(define multiparty
  (make-multiparty 5))

(for-each (lambda (player)
	    (let ((r (random 2)))
	      (begin
	    (pp "Some player voted")
	    (pp r)
	    (set-player-info! player 'input r))))
	  multiparty)

(distributed-sum multiparty)
