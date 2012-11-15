#lang racket
(require rackunit
         future-visualizer/dsl
         future-visualizer/private/display
         (rename-in slideshow/pict
                    [rectangle pict-rectangle]
                    [circle pict-circle]))

(define (make-up-vregion)
  (viewable-region 0 0 (random 400) (random 400)))

(define (views-have-bounds? root-view)
  (cond 
    [(not (view-bounds root-view)) #f]
    [else 
     (for/and ([child (in-list (view-children root-view))])
       (views-have-bounds? child))]))

(define v1-builder (build-view 'view1
                       #:layout (λ (view vregion)
                                  (blank 400 400))))

(check-true (procedure? v1-builder))
(check-equal? (procedure-arity v1-builder) 2)
(define v1 (v1-builder #f #f))
(check-true (view? v1))
(check-equal? (view-name v1) 'view1)
(check-true (null? (view-nodes v1)))
(check-true (null? (view-interactions v1)))

(define v2-builder (build-view 'view2
                               #:nodes (λ (data) '(1 2 3 4 5))
                               #:out-edges (λ (n)
                                             (case n
                                               [(1) '(2 3)]
                                               [else '()]))
                               #:node-view (circle #:diameter 20
                                                   #:back-color "red")
                               #:edge-view (edge-line)
                               #:layout (tree #:margin 10)))
(check-true (procedure? v2-builder))
(check-equal? (procedure-arity v2-builder) 2)
(define v2 (v2-builder #f #f))
(check-equal? (view-name v2) 'view2)
(check-equal? (length (view-nodes v2)) 5)
(check-equal? (length (filter (λ (nd) (not (null? (node-out-edges nd)))) 
                              (view-nodes v2)))
              1)
(check-true (procedure? (view-layout-drawer v2)))
(define vr (make-up-vregion))
(check-true (pict? ((view-layout-drawer v2) vr)))
(check-true (rect? (view-bounds v2)))
(check-equal? (rect-w (view-bounds v2)) (viewable-region-width vr))
(check-equal? (rect-h (view-bounds v2)) (viewable-region-height vr))
(check-equal? (length (view-children v2)) 7) ;5 node subviews + 2 edge subviews
(check-true (views-have-bounds? v2))



















