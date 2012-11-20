#lang racket/base
(require racket/contract
         (only-in racket/function identity)
         "private/fizz-core.rkt" 
         "private/fizz-builtins.rkt"
         "private/fizz-gui.rkt"
         "private/display.rkt"
         (only-in slideshow/pict pict?))
(provide (contract-out 
          [define-view (->*
                        (symbol?)
                        (#:nodes (any/c . -> . (listof any/c))
                         #:out-edges (any/c . -> . (listof any/c))
                         #:node-view (view? any/c . -> . view?)
                         #:edge-view ((or/c view? #f) node? node? . -> . view?)
                         #:scale-to-canvas? boolean?
                         #:layout (view? (or/c viewable-region? #f) . -> . (or/c pict? #f)))
                        #:rest (listof interaction?)
                        ((or/c view? #f) any/c . -> . view?))]
          [visualize (->* ()
                          (#:width exact-nonnegative-integer?
                           #:height exact-nonnegative-integer?)
                          #:rest (listof view?)
                          void?)])
         (except-out (all-from-out "private/fizz-core.rkt") build-view)
         (all-from-out "private/fizz-builtins.rkt"))

(define (define-view name
                     #:layout [layout (tree #:margin 10)]
                     #:nodes [nodes (λ (data) '())]
                     #:out-edges [out-edges (λ (data) '())]
                     #:node-view [node-view (circle #:diameter (auto 10)
                                                    #:back-color "blue"
                                                    #:fore-color "white"
                                                    #:text identity)]
                     #:edge-view [edge-view (edge-line)]
                     #:scale-to-canvas? [scale-to-canvas? #f] 
                     . interactions)
  (apply build-view 
         name
         interactions
         #:layout layout
         #:nodes nodes
         #:out-edges out-edges
         #:node-view node-view
         #:edge-view edge-view
         #:scale-to-canvas? scale-to-canvas?))
          
          
                     
             
          
