#lang racket
;; cart product task

(define (get-pairs e l) (map (lambda(x) (list e x)) l))
;;(get-pairs 3 '(5 6 7))

(define (cart-prod l1 l2) (apply append (map (lambda (x) (get-pairs x l2)) l1)))
(cart-prod '(1 2 3) '(4 5 6) )
