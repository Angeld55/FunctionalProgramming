#lang racket

;; Function to generate all bitstrings with length == n
;; We recursively get all bitstring with length == (n - 1) and for each of them we add 0 at the start or 1 at the start
;; ((0 0) (0 1) (1 0) (1 1)) -> added zeros: ((0 0 0) (0 0 1) (0 1 0) (0 1 1)), added ones: ((1 0 0) (1 0 1) (1 1 0) (1 1 1))
;; We then append both lists and return them as a result. The base is a list with the empty list (the only bitstring with length == 0)
(define (gen-bitstrings n)
  (if (= n 0)
      '(())
      (append (map (lambda(list) (cons 0 list)) (gen-bitstrings (- n 1))) (map (lambda(list) (cons 1 list)) (gen-bitstrings (- n 1))))))


;; Function to get a sublist from a bistring mask (we assume that their lengths are the same!!!)
(define (get-sublist-bitstring list bs)
   (if (= 0 (length list)) ;; list and bs have equal length
       '()
       (if (= (car bs) 1) ;; Indicates if the first element in the list should be present
           (cons (car list) (get-sublist-bitstring (cdr list) (cdr bs))) ;; Take the first and concat to the rest
           (get-sublist-bitstring (cdr list) (cdr bs))))) ;; Just take the rest
 

(define (get-sublists-pred list p?)
 (filter p? (map (lambda (bs) (get-sublist-bitstring list bs)) (gen-bitstrings (length list)))))
 
;; Example:
;; (get-sublists-pred '(1 2 3 4 5) (lambda (x) (= (apply + x) 6)) )