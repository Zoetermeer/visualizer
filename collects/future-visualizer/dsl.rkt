#lang racket/base
(require racket/contract
         (only-in racket/function identity)
         (only-in racket/class is-a?/c)
         (only-in racket/draw color%)
         "private/fizz-core.rkt" 
         "private/fizz-shapes.rkt"
         "private/fizz-layout.rkt"
         "private/fizz-helpers.rkt"
         "private/fizz-gui.rkt"
         "private/display.rkt"
         (only-in slideshow/pict pict?))
(provide (contract-out 
          [view (->*
                 ((or/c (any/c . -> . (listof _node?)) (listof any/c)))
                 (#:edges (_node? (listof _node?) . -> . (listof _edge?))
                  #:scale-to-bounds boolean? 
                  #:back-color string?
                  #:fore-color string?
                  #:stroke-width exact-nonnegative-integer?
                  #:stroke-color string?
                  (_node? . -> . void?))
                 #:rest (listof _interaction?)
                 (any/c . -> . _view?))]
          [visualize (->* ()
                          (#:width exact-nonnegative-integer?
                           #:height exact-nonnegative-integer?)
                          #:rest (listof _view?)
                          void?)]
          [nodes (->* 
                  ((or/c (listof any/c) (any/c . -> . (listof any/c))))
                  (#:shape (any/c . -> . _element?)
                   #:foreach (or/c #f (any/c . -> . (listof any/c)))) 
                  (any/c . -> . (listof _node?)))])
         (except-out (all-from-out "private/fizz-shapes.rkt") view nodes)
         (all-from-out "private/fizz-helpers.rkt")
         (all-from-out "private/fizz-layout.rkt"))