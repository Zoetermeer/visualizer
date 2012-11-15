#lang racket/base
(require racket/contract
         "private/fizz-syntax.rkt"
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
                        #:edge-view (node? node? . -> . view?)
                        #:scale-to-canvas? boolean?)
                       #:rest (listof interaction?)
                       ((or/c view? #f) any/c . -> . view?))])
         (all-from-out "private/fizz-core.rkt")
         (all-from-out "private/fizz-builtins.rkt")
         (all-from-out "private/fizz-gui.rkt"))