#lang racket/base
(require "fizz-core.rkt") 
(provide layout)

;Apply lt if a function with 0 arity, 
;so we can say (layout tree) instead 
;of (layout (tree)). 
(define (layout lt)
  (cond 
    [(zero? (procedure-arity lt)) (lt)]
    [else lt]))