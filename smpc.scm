;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Secure Multi-Party Computation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type player
  (make-player info)
  player?
  (info get-info set-info!))

(define (get-player-info player tag)
  (assq tag (get-info player)))
 

(define (set-player-info! player tag value)
  (let ((p (assq tag (get-info player))))
    (if p
      (set-cdr! p value)
      (set-info! player
		 (cons (cons tag value)
		       (get-info player))))))

(define (append-player-info! player tag value)
  (let ((p (assq tag (get-info player))))
    (if p
      (set-cdr! p (cons value (cdr p)))
      (set-info! player
		 (cons (cons tag value)
		       (get-info player))))))

(define (receive receiver message)
  (let ((tag (car message))
	(value (cdr message)))
    (append-player-info! receiver tag value)))

(define (send receiver message)
  (receive receiver message))

(define distributed-sum
  (lambda (multiparty)
    (for-each
      (lambda (player_i)
	(let* ((input (get-player-info player 'input))
	       (num-players (length multiparty))
	       (prime (random-k-digit-number 3))
	       (P_i (generate-rand-poly input  num-players prime) 

;;(define smpc-system
;;  (lambda (n p)
