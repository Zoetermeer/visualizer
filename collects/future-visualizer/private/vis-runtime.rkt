#lang racket
(require slideshow/pict)
(provide current-visualization
         current-visualization-data
         current-node-data
         (struct-out visualization)
         (struct-out node)
         (struct-out rect-view)
         (struct-out exec)
         (struct-out future)
         (struct-out thread)
         (struct-out event))

(struct visualization (name nodes layout) #:transparent)
(struct node (data [connections #:mutable] [view #:mutable]) #:transparent)

(struct rect-view (get-width get-height get-color) #:transparent)

(struct exec (start-time end-time all-futures))
(struct future (id real-time spawned-futures barricades syncs allocs) #:transparent)
(struct thread (id all-events))
(struct event (index start-time end-time previous-event next-event))

(define current-visualization (make-parameter #f))
(define current-visualization-data (make-parameter #f))
(define current-node-data (make-parameter #f))