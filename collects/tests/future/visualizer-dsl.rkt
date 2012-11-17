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
                                  (interaction 'hover (circle #:diameter 100
                                                              #:back-color "red"
                                                              #:fore-color "white" 
                                                              #:text "I am a hovering circle!"))
                                  (interaction 'hover (circle #:diameter 50
                                                              #:back-color "green"
                                                              #:fore-color "white" 
                                                              #:text "I am another hovering circle!")))
              #:edge-view (edge-line)
              #:layout (tree)))
(define v4 (v4-builder #f #f))
(check-equal? (length (view-children v4)) 8)
(for ([nd (in-list (view-nodes v4))])
  (check-equal? (length (view-interactions (node-view nd))) 2)
  (for ([int (in-list (view-interactions (node-view nd)))])
    (check-true (interaction? int))
    (check-true (view? (interaction-view int)))
    (check-true (view? (view-parent (interaction-view int))))))
(define dist-nviews (list->seteq (map node-view (view-nodes v4))))
(check-true ((set-count dist-nviews) . > . 1))
(define all-inters (flatten (map (λ (v) (view-interactions v)) (set->list dist-nviews))))
;Make sure interaction views have the right parents
(define dist-views (for/seteq ([i (in-list all-inters)])
                       (interaction-view i)))
(check-equal? (set-count dist-views) (* 2 (length (view-nodes v4))))
(define dist-parents (for/seteq ([i (in-list all-inters)])
                       (view-parent (interaction-view i))))
(check-true ((set-count dist-parents) . > . 1))

(define myView-builder 
  (build-view 'mySuperView
              #:nodes (λ (n) (build-list n identity))
              #:node-view (circle #:diameter (auto 20) 
                                  #:back-color "blue"
                                  #:fore-color "red"
                                  #:text "a"
                                  (interaction 'hover (circle #:diameter 100 
                                                              #:back-color "blue")))
              #:out-edges (λ (x) (list (+ x 1)))
              #:edge-view (edge-line)
              #:layout (stack #:orientation 'horizontal)))
(define mv (myView-builder #f 10))
(for ([nd (in-list (view-nodes mv))])
  (case (node-data nd)
    [(9) (check-equal? (node-out-edges nd) '())]
    [else (check-equal? (length (node-out-edges nd)) 1)]))



                                  


















