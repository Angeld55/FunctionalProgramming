#lang racket

;; Alternative base for the path generation function
;;; (define (isLeaf? tree)
;;;   (if (null? tree)
;;;       #f
;;;       (and (null? (cadr tree)) (null? (caddr tree)))))

;; A tree is a list of three elements: (<root element> <left tree> <right tree>)
;; We generate each path by finding recursively the paths of the left and of the right tree
;; To each of those paths we add at the start the current root element's value
;; That would normally generate all paths from the root to the leaves
;; We want the paths from the root to all nodes so on each step we also add the path containing only the current root element
(define (generate-tree-paths bt)
    (if (null? bt) ;; Alternative base -> if bt is leaf we return `((car bt)). But we must check if left and right exist in that case!!!
        `()
        (let* (
                (root (car bt))
                (left (cadr bt))
                (right (caddr bt))
                (left-paths (generate-tree-paths left))
                (right-paths (generate-tree-paths right))
                (all-subpaths (append left-paths right-paths))
            )
            (append (map (lambda (path) (cons root path)) all-subpaths) (list (list root)))
        )
    )    
)

;; Find product only of odd elements
(define (odd-prod path)
    (apply * (filter odd? path))) ;; Alternative: (foldr * 1 (filter odd? path))

(define (foldr1 op l)
  (if (null? (cdr l)) (car l)
      (op (car l) (foldr1 op (cdr l)))))

;; We have path triples (<product of odd elements in path> <length of path> <path>)
;; We use foldr1 to take the one with least product of odd elements
;; If two of them have the same product - we compare by length
(define (get-min-prod-max-len path-triples-list)
  (foldr1 (lambda (el acc)
            (cond ((< (car el) (car acc)) el)
                   ((> (car el) (car acc)) acc)
                   (else (if (> (cadr el) (cadr acc)) el acc))
            )) 
    path-triples-list)
)

;; For the tree we generate all paths from the root to any node
;; We map each path to a triple -> (<product of odd elements in path> <length of path> <path>)
;; We then use find our target triple and return as a result its path (the third element - caddr)
(define (minimum-odd-nodes-prod bt)
  (caddr (get-min-prod-max-len (map (lambda (path) (list (odd-prod path) (length path) path)) (generate-tree-paths bt)))))