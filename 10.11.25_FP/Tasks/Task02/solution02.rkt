#lang racket

(define (get-num-len n)
  (if (< n 10) ;; We use this base case to avoid additional check for 0
      1
      (+ 1 (get-num-len (quotient n 10)))))

(define (sum-with-position n)
    ;; "Helper" function that adds a position counter as argument
    (define (sum-with-current-position curr pos)
        (if (< curr 10) ;; An alternative base case is (= n 0) but then we need to make sure the startin n is not 0
            (+ curr pos)
            (let* (
                    (prefix (sum-with-current-position (quotient curr 10) (+ pos 1)))
                    (next-num (+ pos (remainder curr 10)))
                    (next-num-len (get-num-len next-num))
                )
                (+ (* (expt 10 next-num-len) prefix) next-num) 
            )
        )
    )

    (sum-with-current-position n 1)
)
