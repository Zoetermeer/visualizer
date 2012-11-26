#lang racket/base
(require racket/contract
         (only-in racket/function identity)
         "private/fizz-core.rkt" 
         ;"private/fizz-builtins.rkt"
         "private/fizz-gui.rkt"
         "private/display.rkt"
         (only-in slideshow/pict pict?))
(provide (contract-out 
          [view (->*
                 ((any/c . -> . (listof _node?)))
                 ((any/c . -> . (listof any/c))
                  (any/c . -> . (listof any/c))
                  boolean? 
                  (_node . -> . void?))
                 #:rest (listof _interaction?)
                 ((any/c . -> . _node?)))]
          [visualize (->* ()
                          (#:width exact-nonnegative-integer?
                           #:height exact-nonnegative-integer?)
                          #:rest (listof _view?)
                          void?)])
         (except-out (all-from-out "private/fizz-core.rkt") view))
          
          
                     
             
          
