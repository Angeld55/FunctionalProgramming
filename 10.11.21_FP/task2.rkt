#lang racket

(define (get-neihbors l s) (filter (lambda (x) (= (car x) (cdr s))) (remove s l) ))


(define (get-longest-list l)
  (foldr (lambda (a b) (if ( > ( length a) (length b)) a b)) '() l))

(define (get-longest-path-from l s)
  (if (empty?  (get-neihbors l s))
      (list s)
      (get-longest-list (map  (lambda (x) (cons s (get-longest-path-from (remove s (remove x l)) x)))  (get-neihbors l s)))))

(define (longest-path l) (get-longest-list (map (lambda (x) (get-longest-path-from (remove x l) x )) l)))
      

;;(longest-path '( (1 . 4) (9 . 1) (6 . 7) (4 . 6) (7 . 4) (1 . 5) (4 . 8) (6 . 1) (8 . 7)) )
