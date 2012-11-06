#lang racket/base
(provide current-visualization
         current-visualization-data
         current-node-data
         (struct-out view)
         (struct-out node)
         node-width
         node-height
         control-point
         (struct-out edge)
         (struct-out exec)
         (struct-out future)
         (struct-out thread)
         (struct-out event))

(struct view (name nodes layout) #:transparent)
(struct node (data 
              [in-edges #:mutable] ;(listof edge)
              [out-edges #:mutable] ;(listof edge)
              [view-drawer #:mutable #:auto] ;(any -> pict)
              [x #:mutable #:auto] ;exact-nonnegative-integer
              [y #:mutable #:auto]) ;exact-nonnegative-integer
  #:transparent)

(define (node-width node) 
  0)

(define (node-height node) 
  0)

;;control-point : node symbol symbol -> (values uint uint)
(define (control-point node horiz vert) (values 0 0))

(struct edge (tail-node ;start node  (tail-node -----> head-node)
              head-node ;end node
              [view-drawer #:mutable #:auto]) ;(node node -> pict)
  #:transparent)

(struct exec (start-time end-time all-futures))
(struct future (id real-time spawned-futures barricades syncs allocs) #:transparent)
(struct thread (id all-events))
(struct event (index start-time end-time previous-event next-event))

(define current-visualization (make-parameter #f))
(define current-visualization-data (make-parameter #f))
(define current-node-data (make-parameter #f))