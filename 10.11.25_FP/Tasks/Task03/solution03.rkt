#lang racket

;; Example implementation 1:
;; (apply <function that takes n arguments> <list with n arguments>)
;; Passes the list values all at once as arguments to the function 
(define (row-min function-row num-list)
  (apply min (apply append (map (lambda (fn) (map (lambda (n) (fn n)) num-list)) function-row))))
 
;; Example implementation 2:
;; (foldr <function that takes 2 arguments> <initial value> <list with n arguments>)
;; Performs the function on the last element and the initial value. The result overwrites the initial value and repeats the process again with the remaining elements 
;;(define (row-min function-row num-list)
;;  (apply min (foldr append `() (map (lambda (fn) (map (lambda (n) (fn n)) num-list)) function-row))))

;; Here we use apply because we don't have initial value
;; An alternative is foldr1 which takes the last element as initial value
(define (maxmin fm l)
  (apply max (map (lambda (fList) (row-min fList l)) fm)))