;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Secure Multi-Party Computation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A type for a person using the
;; system. Each player has a single
;; attribute 'info' which is an a-list
;; that contains all of his/her
;; information about the computation
;; to be made including his/her
;; input and any messages from
;; other players.
(define-record-type player
  (make-player info)
  player?
  (info get-info set-info!))

;; Retreive the value associated with
;; a given tag from a player.
(define (get-player-info player tag)
  (assq tag (get-info player)))
 
;; Set the value for a given tag in
;; the info a-list for a player.
(define (set-player-info! player tag value)
  (let ((p (assq tag (get-info player))))
    (if p
      (set-cdr! p value)
      (set-info! player
		 (cons (cons tag value)
		       (get-info player))))))

;; Append the value to the existing
;; value for a given tag in the
;; info a-list for a player.
(define (append-player-info! player tag value)
  (let ((p (assq tag (get-info player))))
    (if p
      (set-cdr! p (cons value (cdr p)))
      (set-info! player
		 (cons (list tag value)
		       (get-info player))))))

;; Simulate the receipt of a message.
;; The player adds the message
;; to his/her info a-list.
(define (receive receiver message)
  (let ((tag (car message))
	(value (cadr message)))
    (append-player-info! receiver tag value)))

;; Simulate the process of sending 
;; a message to another player.
(define (send receiver message)
  (receive receiver message))

;; Make a group of players with no
;; information.
(define (make-multiparty n)
  (let lp ((multiparty '())
	   (n n))
    (if (= n 0)
      multiparty
      (lp (cons (make-player '())
		multiparty)
	  (- n 1)))))

;; Compute the sum of inputs for a
;; given multiparty.
(define distributed-sum
  (lambda (multiparty)
    (define prime (random-k-digit-prime 3))
    (define num-players (length multiparty))
    (define rand-list (generate-rand-list num-players prime))
    ;; Players create their secrets P_i, R_i,
    ;; and share the pieces with other players
    (for-each
      (lambda (player_i)
	(let* ((input (cdr (get-player-info player_i 'input)))
	       (P_i (generate-rand-poly input (- num-players 1) prime)) 
	       (p_shares (point-value-poly P_i rand-list prime))
	       (R_i (generate-rand-poly 0 (- num-players 1) prime))
	       (r_shares (point-value-poly R_i rand-list prime)))
	  (let lp ((players (list-copy multiparty))
		   (p_shares p_shares)
		   (r_shares r_shares))
	    (if (not (eq? players '()))
	      ;; Loop to send shares
	      (begin
		(send (car players) (list 'P (car p_shares)))
		(send (car players) (list 'R (car r_shares)))
		(lp (cdr players)
		    (cdr p_shares)
		    (cdr r_shares)))))))
      multiparty)
    ;; Results are accumulated. In a real scenario
    ;; over a network, the players would do this 
    ;; computation independently and share the results
    ;; (which give no information about any of the initial
    ;; inputs)
    (let lp2 ((players (list-copy multiparty))
	      (rand-list (list-copy (reverse rand-list)))
	      (points '()))
      (if (eq? players '())
	(reconstruct-poly points 0 prime)
	(let* ((player_i (car players))
	       (p_shares (cdr (get-player-info player_i 'P)))
	       (r_shares (cdr (get-player-info player_i 'R))))
	  (lp2 (cdr players)
	       (cdr rand-list)
	       (cons 
		 (cons (car rand-list)
		       (apply + (append p_shares r_shares)))
		 points)))))))
