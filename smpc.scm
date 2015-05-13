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
  (make-player name id input r-values h-values other)
  player?
  (name get-name set-name!)
  (id get-id set-id!)
  (input get-input set-input!)
  (r-values get-r-values set-r-values!)
  (h-values get-h-values set-h-values!)
  (other get-other set-other!))

;; Retreive the value associated with
;; a given tag from a player.
(define (get-player-r-values player tag)
  (assoc tag (get-r-values player)))

;; Set the value for a given tag in
;; the info a-list for a player.
(define (set-player-r-values! player tag value)
  (let ((p (assoc tag (get-r-values player))))
    (if p
      (set-cdr! p value)
      (set-r-values! player
		 (cons (cons tag value)
		       (get-r-values player))))))

;; Append the value to the existing
;; value for a given tag in the
;; info a-list for a player.
(define (append-player-r-values! player tag value)
  (let ((p (assoc tag (get-r-values player))))
    (if p
      (set-cdr! p (cons value (cdr p)))
      (set-r-values! player
		 (cons (list tag value)
		       (get-r-values player))))))

;; Retreive the value associated with
;; a given tag from a player.
(define (get-player-h-values player tag)
  (assoc tag (get-h-values player)))

;; Set the value for a given tag in
;; the info a-list for a player.
(define (set-player-h-values! player tag value)
  (let ((p (assoc tag (get-h-values player))))
    (if p
      (set-cdr! p value)
      (set-h-values! player
		 (cons (cons tag value)
		       (get-h-values player))))))

;; Append the value to the existing
;; value for a given tag in the
;; info a-list for a player.
(define (append-player-h-values! player tag value)
  (let ((p (assoc tag (get-h-values player))))
    (if p
      (set-cdr! p (cons value (cdr p)))
      (set-h-values! player
		 (cons (list tag value)
		       (get-h-values player))))))

;; Retreive the value associated with
;; a given tag from a player.
(define (get-player-other player tag)
  (assoc tag (get-other player)))
 
;; Set the value for a given tag in
;; the info a-list for a player.
(define (set-player-other! player tag value)
  (let ((p (assoc tag (get-other player))))
    (if p
      (set-cdr! p value)
      (set-info! player
		 (cons (cons tag value)
		       (get-other player))))))

;; Append the value to the existing
;; value for a given tag in the
;; info a-list for a player.
(define (append-player-other! player tag value)
  (let ((p (assoc tag (get-other player))))
    (if p
      (set-cdr! p (cons value (cdr p)))
      (set-other! player
		 (cons (list tag value)
		       (get-other player))))))

;; Simulate the receipt of a message.
;; The player adds the message
;; to his/her info a-list.
(define (receive receiver message message-type)
  (let ((tag (car message))
	(value (cadr message)))
    (cond ((eq? message-type 'r-value)
	   (append-player-r-values! receiver tag value))
	  ((eq? message-type 'h-value)
	   (append-player-h-values! receiver tag value))
	  ((eq? message-type 'other)
	   (append-player-other! receiver tag value))
	  (else
	    (error "Unsupported message type")))))

;; Simulate the process of sending 
;; a message to another player.
(define (send receiver message message-type)
  (receive receiver message message-type))

;; Make a group of players with no
;; information.
(define (make-multiparty n)
  (let lp ((multiparty '())
	   (n n))
    (if (= n 0)
      multiparty
      (lp (cons (make-player 'name 'id 'input '() '() '())
		multiparty)
	  (- n 1)))))

(define secret-share
  (lambda (multiparty sender-index value tag prime message-type)
    (define num-players (length multiparty))
    (define P_i (generate-rand-poly value (- num-players 1) prime))
    (pp "sharing")
    (pp sender-index)
    (pp value)
    (pp P_i)
    (for-each
      (lambda (player_i)
	(let* ((index (get-id player_i))
	       (share (poly-eval P_i index prime)))
	  (pp "each share")
	  (pp index)
	  (pp share)
	  (if (eq? message-type 'h-value)
	    (send player_i (list (cons tag sender-index) share)
		  message-type)
	    (send player_i (list tag share) message-type))))
      multiparty)))

(define broadcast
  (lambda (multiparty value tag message-type)
    (for-each
      (lambda (player_i)
	(send player_i (list tag value) message-type))
      multiparty)))

(define secret-share-inputs
  (lambda (multiparty prime)
    (for-each
      (lambda (player_i)
	(let ((input (get-input player_i))
	      (index (get-id player_i)))
	  (secret-share multiparty index input index prime 'other)))
      multiparty)))

(define get-input-shares
  (lambda (player_i input-tags)
    (let lp ((in input-tags)
	     (shares '()))
      (if (eq? in '())
	shares
	(lp (cdr in)
	    (cons (cadr (get-player-other player_i (car in)))
		  shares))))))

;; Compute the sum of inputs for a
;; given multiparty.
(define distributed-sum
  (lambda (multiparty input-tags output-tag prime)
    (define num-players (length multiparty))
    ;; Players create their secrets P_i, R_i,
    ;; and share the pieces with other players
    (for-each
      (lambda (player_i)
	(secret-share multiparty
		      (get-id player_i)
		      0
		      output-tag
		      prime
		      'r-value))
      multiparty)
    ;; Results are accumulated. In a real scenario
    ;; over a network, the players would do this 
    ;; computation independently and share the results
    ;; (which give no information about any of the initial
    ;; inputs)
    (for-each
      (lambda (player_i)
	(let ((index (get-id player_i))
	      (p_shares (get-input-shares player_i input-tags))
	      (r_shares
		(cdr
		  (get-player-r-values player_i output-tag))))
	  (broadcast multiparty
		     (cons index
			   (apply + (append p_shares r_shares)))
		     'output
		     'other)))
      multiparty)
    (for-each
      (lambda (player_i)
	(let ((points (cdr (get-player-other player_i 'output))))
	  (pp "Player computed: ")
	  (pp (reconstruct-poly points 0 prime))))
      multiparty)))

(define get-player-indices
  (lambda (multiparty)
    (let lp ((players multiparty)
	     (indices '()))
      (if (eq? players '())
	indices
	(lp (cdr players) (cons (get-id (car players)) indices))))))

(define get-h-shares
  (lambda (player_i tag indices)
    (let lp ((indices indices)
	     (h_pairs '()))
      (if (eq? indices '())
	h_pairs
	(let* ((h-tag (cons tag (car indices)))
	       (h-value (get-player-h-values
			  player_i h-tag)))
	(lp (cdr indices)
	    (cons (cons (cdar
			  (get-player-h-values
			    player_i
			    h-tag))
			(cadr
			  (get-player-h-values
			    player_i
			    h-tag)))
		  h_pairs)))))))

(define compute-lambda_i
  (lambda (current_index indices prime)
        (define (mod-inv p)
	        (lambda (a) 
		        (modulo (expt a (- p 2)) p)))
    (let lp ((indices indices)
	     (value 1))
      (if (eq? indices '())
	value
	(if (= (car indices) current_index)
	  (lp (cdr indices) value)
	  (let* ((a_j (car indices))
;;		 (a_i-a_j^-1 ((inversemod prime)
;;			      (-mod
;;				current_index
;;				a_j
;;				prime))))
                (a_i-a_j^-1 ((mod-inv prime) 
			      (-mod
				current_index
				a_j
				prime))))
	    (lp (cdr indices) (*mod
			      (*mod
				(- a_j)
				a_i-a_j^-1
				prime)
			     value prime))))))))

(define combine-h-values
  (lambda (h_shares indices prime)
    (let lp ((h_pairs (list-copy h_shares))
	     (value 0))
      (if (eq? h_pairs '())
	value
	(let* ((current_index (caar h_pairs))
	       (current_value (cdar h_pairs))
	       (lambda_i (compute-lambda_i current_index
					   indices prime)))
	  (pp "(index lambda h_value)")
	  (pp (list current_index lambda_i current_value))
	  (lp (cdr h_pairs)
	    (+ (* lambda_i
		 current_value)
	       value)))))))


;; Compute the sum of inputs for a
;; given multiparty.
(define distributed-multiply
  (lambda (multiparty input-tags output-tag prime)
    (define num-players (length multiparty))
    ;; Players create their secrets H_i,
    ;; and share the pieces with other players
    (for-each
      (lambda (player_i)
	(let ((input_shares
		(get-input-shares player_i input-tags)))
	  (secret-share
	    multiparty
	    (get-id player_i)
	    (apply * input_shares)
	    output-tag
	    prime
	    'h-value)))
      multiparty)
    ;; Results are accumulated. In a real scenario
    ;; over a network, the players would do this 
    ;; computation independently and share the results
    ;; (which give no information about any of the initial
    ;; inputs)
    (for-each
      (lambda (player_i)
	(let* ((index (get-id player_i))
	      (indices (get-player-indices multiparty))
	      (h_shares
		(get-h-shares player_i output-tag indices)))
	  (pp "broadcasting: ")
	  (pp (list index (combine-h-values h_shares indices prime)))

	  (broadcast multiparty
		     (cons index
			   (combine-h-values
			     h_shares indices prime))
		     'output
		     'other)))
      multiparty)
    (for-each
      (lambda (player_i)
	(let ((points (cdr (get-player-other player_i 'output))))
	  (pp "Player computed: ")
	  (pp (reconstruct-poly points 0 prime))))
      multiparty)))
