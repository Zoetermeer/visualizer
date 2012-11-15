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

(define (nodes-have-views? view)
  (for/and ([nd (in-list (view-nodes view))])
    (view? (node-view nd))))

(define (default-view 
          name
          #:nodes nodes 
          #:out-edges out-edges 
          #:layout layout)
  (build-view name 
              #:nodes nodes
              #:out-edges out-edges
              #:node-view (circle #:diameter 30
                                  #:back-color "red")
              #:edge-view (edge-line)
              #:layout layout))

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
(check-true (nodes-have-views? v2))

(define inv-child-builder (build-view 'badv
                                      #:nodes (λ (data) '(1 2 3 4 5))
                                      #:out-edges (λ (x)
                                                    (case x 
                                                      [(1) '(5 6)]
                                                      [else '()]))
                                      #:node-view (circle #:diameter 20
                                                          #:back-color "black")
                                      #:edge-view (edge-line)
                                      #:layout (tree)))
(check-exn exn:fail? (λ () (inv-child-builder #f #f))) ;Invalid child

;Need to test for cycle detection when using tree
;layout, but arbitrary graphs are otherwise okay
(define cycle-builder (build-view 'with-cycles
                                  #:nodes (λ (data) '(1 2 3 4 5))
                                  #:out-edges (λ (x)
                                                (case x 
                                                  [(1) '(2)]
                                                  [(2) '(1)] 
                                                  [else '()]))
                                  #:node-view (circle #:diameter 20
                                                      #:back-color "black")
                                  #:edge-view (edge-line)
                                  #:layout (tree)))

;Try creating a view with nodes but no node view
(define v3-builder (build-view 'view3
                               #:nodes (λ (data) '(1 2 3 4 5))
                               #:out-edges (λ (x)
                                             (case x 
                                               [(4) '(2 3)]
                                               [(2) '(1)]
                                               [else '()]))
                               #:edge-view (edge-line)
                               #:layout (tree)))
(check-exn exn:fail? (λ () (v3-builder #f #f)))

;Simple interaction view for testing
(define (offset-circle offx offy)
  (λ (parent-view data) 
    ((circle #:diameter 100
             #:back-color "red"
             #:fore-color "white"
             #:text "I am a hovering circle!"
             #:x (+ (view-origin-x parent-view) offx)
             #:y (+ (view-origin-y parent-view) offy)) 
     parent-view data)))

(define v4-builder 
  (build-view 'view4
              #:nodes (λ (data) '(1 2 3 4 5))
              #:out-edges (λ (x)
                            (case x 
                              [(4) '(2 3)]
                              [(2) '(1)]
                              [else '()]))
              #:node-view (circle #:diameter 30
                                  #:back-color "blue" 
                                  (interaction 'hover (offset-circle 40 40))
                                  (interaction 'click (offset-circle -40 -40)))
              #:edge-view (edge-line)
              #:layout (tree)))
(define v4 (v4-builder #f #f))
(check-equal? (length (view-children v4)) 8)
(for ([nd (in-list (view-nodes v4))])
  (check-equal? (length (view-interactions (node-view nd))) 2))


                                  


















