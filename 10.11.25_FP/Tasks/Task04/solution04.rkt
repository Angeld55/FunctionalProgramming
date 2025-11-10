#lang racket

;; For each value X in the domain we check if the pair (X . X) is in the relation
(define (reflexive? domain relation) 
    (not (member #f (map (lambda (x) (member (cons x x) relation)) domain))))

;; Symetric is a relation that: for each pair (x . y) that is in the relation, the pair (y . x) is also in it
;; We generate all possible pairs (cartesian-product) of the domain
;; We check for each pair if it is in the relation we check if the reverse pair also is in it
;; What we just described is IMPLICATION (A => B)
(define (symetric? domain relation) 
    (not (member #f (map
                        (lambda(pair) (implies (member (cons (car pair) (cadr pair)) relation ) (member (cons (cadr pair) (car pair)) relation )))
                        (cartesian-product domain domain) ;; Returns all pairs from elements in the domain. It returns them as list of lists of two elements
                    )
    ))
)
;; Alternative lambda:
;; (lambda(pair) (if (member (cons (car pair) (cadr pair)) relation)
;;                   (member (cons (cadr pair) (car pair)) relation)
;;                   #t)
;; )

;; Transitive is a relation that: for each pair (x . y) and (y . z) that are in the relation, the pair (x . z) is also in it
;; We generate all possible triples (cartesian-product) of the domain
;; We check for each triple (X Y Z) if the (X . Y) pair and (Y . Z) pair is in the relation we check if the pair (X . Z) also is in it
(define (transitive? domain relation) 
    (not (member #f (map 
                        (lambda (triple) (implies (and (member (cons (car triple) (cadr triple)) relation) (member (cons (cadr triple) (caddr triple)) relation)) (member (cons (car triple) (caddr triple)) relation))) 
                        (cartesian-product domain domain domain)
                    )
    ))
)
;; Alternative lambda:
;; (lambda(triple) (if (and (member (cons (car triple) (cadr triple)) relation) (member (cons (cadr triple) (caddr triple)) relation))
;;                   (member (cons (car triple) (caddr triple)) relation)
;;                   #t)
;; )

(define (is-equivalence-relation domain relation) 
    (and (reflexive? domain relation) (symetric? domain relation) (transitive? domain relation)))

;; A function that return all elements in relation to a target element
(define (get-related-elements element relation) 
    (map 
        (lambda(y) (cdr y)) ;; We take only the elements in relation to the target
        (filter (lambda(x) (= (car x) element)) relation) ;; We take the pairs with first element - the target
    )
)


(define (equivalence-relation domain relation)
  (if (is-equivalence-relation domain relation)
      (remove-duplicates (map (lambda (el) (sort (get-related-elements el relation) < )) domain))
      #f))