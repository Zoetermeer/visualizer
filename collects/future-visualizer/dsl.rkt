#lang racket/base
(require racket/contract
         "private/fizz-core.rkt" 
         "private/fizz-builtins.rkt"
         "private/fizz-gui.rkt"
         "private/display.rkt"
         (only-in slideshow/pict pict?))
(provide (contract-out 
          [build-view (->*
                       (symbol? 
                        #:layout (view? (or/c viewable-region? #f) . -> . (or/c pict? #f)))
                       (#:nodes (any/c . -> . (listof any/c))
                        #:out-edges (any/c . -> . (listof any/c))
                        #:node-view (view? any/c . -> . view?)
                        #:edge-view ((or/c view? #f) node? node? . -> . view?)
                        #:scale-to-canvas? boolean?)
                       #:rest (listof interaction?)
                       ((or/c view? #f) any/c . -> . view?))]
          [visualize (->* ()
                          (#:width exact-nonnegative-integer?
                           #:height exact-nonnegative-integer?)
                          #:rest (listof view?)
                          void?)])
         (except-out (all-from-out "private/fizz-core.rkt") build-view)
         (all-from-out "private/fizz-builtins.rkt"))
