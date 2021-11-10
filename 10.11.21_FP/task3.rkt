#lang racket

;; task 3
(define (reflexive? d r) (not (member #f (map (lambda (x) (member (cons x x) r)) d))))

;;(reflexive? '(1 2 3) '( (3 . 3) (1 . 3) (1 . 1) (2 . 2) ))

(define (in-relation? p r) (member p r ))

(define (symetric? d r)  (not (member #f (map   (lambda(x) (implies (in-relation? (cons (first x) (second x)) r )  (in-relation? (cons (second x) (first x)) r )   ))  (cartesian-product d d )))))

(symetric? '(1 2 3) '( (3 . 2) (2 . 3) (1 . 2) (2 . 1) ))


(define (transitive? d r) (not (member #f (map (lambda (x) (implies (and (in-relation? (cons (first x) (second x)) r) (in-relation? (cons (second x) (third x)) r)  ) (in-relation? (cons (first x) (third x))r))) (cartesian-product d d d)))))

(transitive? '(1 2 3) '( (1 . 2) (2 . 3) (1 . 4)))


(define (is-equivalence-relation d r) (and (reflexive? d r) (symetric? d r) (transitive? d r)))


(define (get-relations e r) (map (lambda(y) (cdr y) )(filter (lambda(x) (= (car x) e)) r)))


(define (equivalence-relation d r)
  (if (is-equivalence-relation d r)
      (remove-duplicates (map (lambda (x) (sort (get-relations x r) < )) d))
      #f))

;;(equivalence-relation '(1 2 3 4 5 6) '( (4 . 4) (5 . 5) (6 . 6) (1 . 2) (2 . 1) (1 . 1) (2 . 2) (3 . 3) (1 . 3) (3 . 2) (3 . 1) (2 . 3)  (4 . 5) (5 . 4) ))
