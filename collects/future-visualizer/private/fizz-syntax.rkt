#lang racket
(require data/interval-map
         (only-in slideshow/pict pict?)
         "fizz-core.rkt"
         "display.rkt")
(provide (contract-out 
          [build-view (->*
                       (symbol? 
                        #:layout-view (any/c (listof node?) (or/c viewable-region? #f) . -> . pict?))
                       (#:nodes (any/c . -> . (listof any/c))
                        #:out-edges (any/c . -> . (listof any/c))
                        #:node-view (any/c (or/c viewable-region? #f) . -> . pict?)
                        #:edge-view (node? node? (or/c viewable-region? #f) . -> . pict?)
                        #:scale-to-canvas? boolean?)
                       #:rest (listof interaction?)
                       (any/c (or/c viewable-region? #f) . -> . pict?))]))

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
                    #:node-view [node-view-builder (λ (data vregion) #f)]
                    #:edge-view [edge-view-builder (λ (tail head vregion) #f)]
                    #:scale-to-canvas? [scale-to-canvas? #f]
                    #:layout-view layout-view-builder
                    . interactions)
  (λ (data vregion)
    (define nds (map (λ (v) (node v '() '())) (nodes data)))
    (for ([n (in-list nds)])
      (define nd (node-data n))
      (set-node-view-drawer! n (λ () (view-layout-drawer (node-view-builder nd vregion))))
      (define outs (out-edges nd))
      (define out-nodes (find-nodes (out-edges nd) nds))
      (for ([o-n (in-list out-nodes)])
        (define e (edge n o-n (λ () (view-layout-drawer (edge-view-builder n o-n vregion)))))
        (set-node-out-edges! n (cons e (node-out-edges n)))
        (set-node-in-edges! o-n (cons e (node-in-edges o-n)))))
    ;layout-view is expected to update nodes' positional information
    (define drawer (view-layout-drawer layout-view-builder))
    (define pct (drawer data nds vregion))
    (view name
          nodes
          data
          drawer
          pct
          #f
          scale-to-canvas?)))









