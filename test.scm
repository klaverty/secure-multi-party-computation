(define multiparty
  (make-multiparty 5))

(for-each (lambda (player)
	    (set-player-info! player 'input 1))
	  multiparty)

(distributed-sum multiparty)

#|
(define player (make-player '()))

(set-player-info! player 'input 1)
(pp  (get-player-info player 'input))
(define p (generate-rand-poly 27 10 19))
(pp p)
(define q (generate-rand-poly 27 10 19))
(pp q)
(define cp (point-value-poly p 19))
(pp cp)
(pp (reconstruct-poly cp 0 19))
(pp (poly-add p q))
|#
