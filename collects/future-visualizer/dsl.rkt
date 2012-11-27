#lang racket/base
(require racket/contract
         (only-in racket/function identity)
         "private/fizz-core.rkt" 
         "private/fizz-gui.rkt"
         "private/display.rkt"
         (only-in slideshow/pict pict?))
(provide (contract-out 
          [view (->*
                 ((or/c (any/c . -> . (listof _node?)) (listof any/c)))
                 (#:edges (_node? (listof _node?) . -> . (listof _edge?))
                  #:scale-to-bounds boolean? 
                  (_node? rect? . -> . void?))
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
         (except-out (all-from-out "private/fizz-core.rkt") view nodes))