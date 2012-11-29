#lang racket
(require rackunit
         future-visualizer/dsl
         future-visualizer/private/display
         future-visualizer/private/fizz-core
         (rename-in slideshow/pict
                    [rectangle pict-rectangle]
                    [circle pict-circle]))

(define (make-up-rect)
  (rect 0 0 (random 400) (random 400)))

(define (elements-have-bounds? root-elem)
  (cond 
    [(not (_element-bounds root-elem)) #f]
    [(_edge? root-elem) #t]
    [else 
     (for/and ([child (in-list (_node-children root-elem))])
       (elements-have-bounds? child))]))

(define v1-builder (view '()
                         (layout tree)))

(check-true (procedure? v1-builder))
(check-equal? (procedure-arity v1-builder) 1)
(define v1 (v1-builder #f))
(check-true (_view? v1))
(check-true (_node? v1))
(check-true (null? (_node-children v1)))

(define v2-builder (view 
                    (nodes '(1 2 3 4 5)
                           #:shape (circle #:diameter 20 #:back-color "red"))
                    #:edges (edges-from (λ (d) 
                                          (case d 
                                            [(1) '(2 3)]
                                            [else '()])) 
                                        #:shape (line))
                    (layout (tree #:padding 10))))
(check-true (procedure? v2-builder))
(check-equal? (procedure-arity v2-builder) 1)
(define v2 (v2-builder #f))
(check-equal? (length (_node-children v2)) 7) ;5 nodes + 2 edges
(check-equal? (length (filter (λ (nd) (and (_node? nd) (null? (_node-from-edges nd)))) 
                              (_node-children v2)))
              4)

(define vr (make-up-rect))
(check-true (pict? (draw v2 vr)))
(check-true (rect? (_element-bounds v2)))
(check-equal? (rect-w (_element-bounds v2)) (rect-w vr))
(check-equal? (rect-h (_element-bounds v2)) (rect-h vr))
(check-equal? (length (_node-children v2)) 7) ;5 node children + 2 edge children
(check-true (elements-have-bounds? v2))

;Need to test for cycle detection when using tree
;layout, but arbitrary graphs are otherwise okay
(define cycle-builder (view 
                       (nodes '(1 2 3 4 5))
                       #:edges (edges-from (λ (x)
                                             (case x 
                                               [(1) '(2)]
                                               [(2) '(1)]
                                               [else '()])))
                       (layout tree)))

;Try creating a view with nodes but no node view
(define v3-builder (view 
                    (nodes '(1 2 3 4 5))
                    #:edges (edges-from (λ (x)
                                          (case x 
                                            [(4) '(2 3)]
                                            [(2) '(1)]
                                            [else '()])))
                    (layout tree)))
(check-not-exn (λ () (v3-builder #f)))


#|
;Simple interaction view for testing
(define v4-builder 
  (define-view 'view4
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
  (define-view 'mySuperView
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

(check-not-exn 
 (λ ()
   (define (build-list size)
     (if (= 0 size) 
         '()
         (cons size (build-list (- size 1)))))
   (define myView (define-view 'mySuperView
                    #:nodes build-list
                    #:node-view (circle #:diameter (auto 20) 
                                        #:back-color "blue"
                                        #:fore-color "red"
                                        #:text "a"
                                        (interaction 'hover (circle #:diameter 100 
                                                                    #:back-color "blue")))
                    #:out-edges (lambda (x) (if (= x 10) '() (list (+ x 1))))
                    #:layout (stack #:orientation 'horizontal)
                    ))
   (myView #f 10)))
|#



                                  


















