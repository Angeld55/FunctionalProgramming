#lang racket

;;task 1
(define (gen-bitstrings n)
  (if (= n 1)
      '((0) (1))
      (append (map  (lambda(x) (cons 0 x))  (gen-bitstrings (- n 1))) (map  (lambda(x) (cons 1 x))  (gen-bitstrings (- n 1))))))

(define (get-sublist l b)
  (if (empty? l)
      '()
      (if(= 0 (car b))
         (get-sublist (cdr l) (cdr b))
         (cons (car l) (get-sublist (cdr l) (cdr b))))))

(define (get-all-sublists l)
      (map  (lambda (x) (get-sublist l x))  (gen-bitstrings (length l))))



(define (get-sublists-pred l p) (filter p (get-all-sublists l)))


;;(get-sublists-pred '(1 2 3 4 5) (lambda (x) (= (apply + x) 6)) )
