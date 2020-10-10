; Poker Project
; final function
(define (poker-value hand)
  (let ((sortedhand (sort hand))
        (c-hand (compute-ranks hand)))
    (cond ((royal-flush? sortedhand)
           (se '(royal flush -) (suit-name (first (first hand)))))
          ((straight-flush? sortedhand)
           (se (highest-card (first sortedhand))'(- straight flush -) (suit-name (first (first hand)))))
          ((four-of-a-kind? sortedhand)
           (se '(four of a kind of) (kind-of-card (get-rank-4 c-hand))))
          ((full-house? sortedhand)
           (se '(full house -) (kind-of-card (get-rank-3 c-hand)) 'over (kind-of-card (get-rank-2 c-hand))))
          ((flush? sortedhand)
           (se '(flush -) (suit-name (first (first hand)))))
          ((straight? sortedhand)
           (se (highest-card (first sortedhand))'(straight)))
          ((three-of-a-kind? sortedhand)
           (se '(three of a kind of) (kind-of-card (get-rank-3 c-hand))))
          ((two-pair? sortedhand)
           (se '(two pairs of) (two-pair-rank c-hand)))
          ((pair? sortedhand)
           (se '(pair of) (kind-of-card (get-rank-2 c-hand))))
          (else (first sortedhand)))))

; helper: converts letter to name
(define (suit-name suit)
  (cond ((equal? suit 'c) 'clubs)
        ((equal? suit 'd) 'diamonds)
        ((equal? suit 'h) 'hearts)
        ((equal? suit 's) 'spades)
        (else 'none)))

; helper: highest card in the hand
(define (highest-card card)
  (cond ((equal? (bf card) 'a) 'ace-high)
        ((equal? (bf card) 'k) 'king-high)
        ((equal? (bf card) 'q) 'queen-high)
        ((equal? (bf card) 'j) 'jack-high)
        ((equal? (bf card) '10) 'ten-high)
        ((equal? (bf card) '9) 'nine-high)
        ((equal? (bf card) '8) 'eight-high)
        ((equal? (bf card) '7) 'seven-high)
        ((equal? (bf card) '6) 'six-high)
        ((equal? (bf card) '5) 'five-high)
        ((equal? (bf card) '4) 'four-high)
        ((equal? (bf card) '3) 'three-high)
        ((equal? (bf card) '2) 'two-high)
        (else 'none)))

; helper: rank in unabbreviated form
(define (kind-of-card card)
  (cond ((equal? card 'a) 'aces)
        ((equal? card 'k) 'kings)
        ((equal? card 'q) 'queens)
        ((equal? card 'j) 'jacks)
        ((equal? card '10) 'tens)
        ((equal? card '9) 'nines)
        ((equal? card '8) 'eights)
        ((equal? card '7) 'sevens)
        ((equal? card '6) 'sixes)
        ((equal? card '5) 'fives)
        ((equal? card '4) 'fours)
        ((equal? card '3) 'threes)
        ((equal? card '2) 'twos)
        (else 'none)))

; helper: rank of cards in a four of a kind
(define (get-rank-4 c-hand)
    (if (empty? c-hand)
        '()
        (if (equal? (first c-hand) 'four)
            (first (bf c-hand))
            (get-rank-4 (bf (bf c-hand))))))

; helper: find the rank of cards in a three of a kind
(define (get-rank-3 c-hand)
  (if (empty? c-hand)
      '()
      (if (equal? (first c-hand) 'three)
          (first (bf c-hand))
          (get-rank-3 (bf (bf c-hand))))))

; helper: find the rank of the cards in a pair
(define (get-rank-2 c-hand)
  (if (empty? c-hand)
      '()
      (if (equal? (first c-hand) 'two)
          (first (bf c-hand))
          (get-rank-2 (bf c-hand)))))

; helper: finds the kind of cards that make up two pairs
(define (two-pair-rank c-hand)
  (cond ((and (equal? (first c-hand) 'two)(equal? (first (bf (bf c-hand))) 'two))
         (se (kind-of-card (first (bf c-hand))) 'and (kind-of-card (first (bf (bf (bf c-hand)))))))
        ((and (equal? (first c-hand) 'two)(equal? (last (bl c-hand)) 'two))
         (se (kind-of-card (first (bf c-hand))) 'and (kind-of-card (last c-hand))))
        ((and (equal? (first (bf (bf c-hand))))(equal? (last (bl c-hand)) 'two))
         (se (kind-of-card (first (bf (bf (bf c-hand))))) 'and (se (kind-of-card (last c-hand)))))))

; helper: checks for a royal flush
(define (royal-flush? hand)
  (and (flush? hand)
       (and (equal? (bf (first hand)) 'a)
            (equal? (bf (last hand)) '10))))

; helper: checks for straight flush
(define (straight-flush? hand)
  (and (flush? hand)
       (or (equal? (numeric-rank (bf (first hand)))
            (+ (numeric-rank (bf (last hand))) 4))
           (and (equal? (bf (first hand)) 'a)
                (equal? (bf (first (bf hand))) 5)))))

; helper: check for four of a kind using other helpers
(define (remove wd sent)
  (if (equal? (count sent) 1)
      (if (equal? wd (first sent))
          '()
          (first sent))
      (se (if (equal? wd (first sent))
              '()
              (first sent))
          (remove wd (bf sent)))))

(define (compute-ranks-helper rank)
  (cond ((empty? rank) '())
        ((equal? (count rank) 1) (se 'one rank))
        (else (se (appearances-num (appearances (first rank) rank)) (first rank)
                  (compute-ranks-helper (remove (first rank) rank))))))

(define (appearances-num num)
  (cond ((equal? num 1) 'one)
        ((equal? num 2) 'two)
        ((equal? num 3) 'three)
        ((equal? num 4) 'four)
        ((equal? num 5) 'five)
        (else 'zero)))

(define (compute-ranks hand)
  (compute-ranks-helper (every bf hand)))

; helper: checks for four of a kind
(define (four-of-a-kind? hand)
  (let ((computed-hand (compute-ranks hand)))
    (member? 'four computed-hand)))

; helper: second version of four of a kind using helpers

(define (first sent)
  (first sent))
(define (second sent)
  (first (bf sent)))
(define (third sent)
  (first (bf (bf sent))))
(define (fourth sent)
  (first (bf (bf (bf sent)))))

(define (four-of-a-kind? hand)
  (cond ((equal? (bf (first hand)) (bf (fourth hand))) #t)
        ((equal? (bf (second hand)) (bf (last hand))) #t)
        (else #f)))

; helper: checks for a full house
(define (full-house? hand)
  (let ((computed-hand (compute-ranks hand)))
    (and (member? 'three computed-hand)(member? 'two computed-hand))))

; checks for a flush
(define (flush? hand)
  (cond ((equal? (count hand) 0) #f)
        ((equal? (count hand) 1) #t)
        ((equal? (first (first hand)) (first (first (bf hand))))
         (flush? (bf hand)))
        (else #f)))

; helper: checks for a straight using helpers
(define (numeric-rank-from-hand hand)
  (let ((rank (every bf hand)))
    (every numeric-rank rank)))

(define (straight-helper hand)
  (let ((rank (numeric-rank-from-hand hand)))
    (if (<= (count hand) 1)
        #t
        (if (equal? (first rank)
                    (+ (first (bf rank)) 1))
            (straight-helper (bf hand))
            #f))))

(define (straight? hand)
  (or (and (equal? (bf (first hand)) 'a)
           (straight-helper (bf hand)))
      (straight-helper hand)))

; helper: checks for three of a kind
(define (three-of-a-kind? hand)
  (let ((computed-hand (compute-ranks hand)))
    (member? 'three computed-hand)))

; helper: second three of a kind
(define (three-of-a-kind? hand)
  (if (= (count hand) 2)
      #f
      (if (equal? (numeric-rank (rank (first hand))) (numeric-rank (rank (third hand))))
          #t
          (three-of-a-kind? (bf hand)))))

; helper: checks for two pair
(define (two-pair? hand)
  (let ((computed-hand (compute-ranks hand)))
    (equal? (appearances 'two computed-hand) 2)))

; helper: checks for pair
(define (pair? hand)
  (let ((computed-hand (compute-ranks hand)))
    (member? 'two computed-hand)))

; helper: second pair code
(define (pair? hand)
  (if (= (count hand) 1)
      #f
      (if (member? (bf (first hand)) (every rank (bf hand)))
          #t
          (pair? (bf hand)))))


; helpful funcionts
(define ranks '(a k q j 10 9 8 7 6 5 4 3 2))
(define suits '(s h d c))

(define (numeric-rank rank)
   (cond ((equal? rank 'a) 14)
         ((equal? rank 'k) 13)
         ((equal? rank 'q) 12)
         ((equal? rank 'j) 11)
         (else rank)))

(define (make-card suit rank)
   (word suit rank))
(define (rank card) (bf card))
(define (suit card) (first card))

(define (sort hand)
    ((repeated sort-once (- (count hand) 1)) hand))

(define (sort-once hand)
  (cond ((empty? hand) hand)
        ((= (count hand) 1) hand)
        ((> (numeric-rank (bf (first hand)))
            (numeric-rank (bf (first (bf hand)))))
         (se (first hand) (sort-once (bf hand))))
        (else (se (first (bf hand))
                  (sort-once (se (first hand) (bf (bf hand))))))))