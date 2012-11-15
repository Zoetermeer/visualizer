#lang racket
(require data/interval-map
         (only-in slideshow/pict pict?)
         "fizz-core.rkt"
         "display.rkt")
(provide build-view)

;Find all nodes n (from nodes) for which (node-data n) is equal 
;to some element of vs.
;;find-nodes : (listof any) (listof node) -> (listof node)
(define (find-nodes vs nodes)
  (define ns (map (λ (v) (findf (λ (n) (equal? (node-data n) v)) nodes)) vs))
  (when (list? (member #f ns))
    (error 'find-nodes "Nodes could not be found for values in the list: ~a" vs))
  ns)

(define (build-view name 
                    #:nodes [nodes (λ (data) '())]
                    #:out-edges [out-edges (λ (node-data) '())]
                    #:node-view [node-view-builder (λ (data) #f)]
                    #:edge-view [edge-view-builder (λ (tail head) #f)]
                    #:scale-to-canvas? [scale-to-canvas? #f]
                    #:layout layout
                    . interactions)
  (λ (parent-view data)
    (define nds (map (λ (v) (node v '() '())) (nodes data)))
    (define vw (view name
                     data
                     parent-view
                     nds
                     #f ;drawer
                     scale-to-canvas?))
    (for ([n (in-list nds)])
      (define nd (node-data n))
      (set-node-view! n (node-view-builder vw nd))
      (define outs (out-edges nd))
      (define out-nodes (find-nodes (out-edges nd) nds))
      (for ([o-n (in-list out-nodes)])
        (define e (edge n o-n (view-layout-drawer (edge-view-builder n o-n))))
        (set-node-out-edges! n (cons e (node-out-edges n)))
        (set-node-in-edges! o-n (cons e (node-in-edges o-n)))))
    (set-view-layout-drawer! vw ((curry layout) vw))
    (set-view-interactions! vw interactions)
    vw))
  







