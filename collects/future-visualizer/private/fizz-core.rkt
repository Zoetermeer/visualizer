#lang racket/base
(require (only-in racket/match match-define))
(provide current-visualization
         current-visualization-data
         current-node-data
         (struct-out view)
         (struct-out node)
         node-width
         node-height
         node-origin-x
         node-origin-y
         node-x-extent
         node-y-extent
         control-point
         (struct-out edge)
         (struct-out interaction)
         (struct-out exec)
         (struct-out future)
         (struct-out thread)
         (struct-out event)
         (struct-out rect))

(struct rect (x y w h) #:transparent)
(struct view (name ;symbol
              data ;any
              nodes ;(listof node)
              [layout-drawers #:mutable] ;(listof (viewable-region -> pict))
              layout-pict ; (or pict #f)
              scale-to-canvas? ;bool
              [hovered-node #:mutable #:auto]
              [selected-node #:mutable #:auto])
  #:transparent)
(struct node (data 
              [in-edges #:mutable] ;(listof edge)
              [out-edges #:mutable] ;(listof edge)
              [view-drawer #:mutable #:auto] ;(any -> pict)
              [view-pict #:mutable #:auto]
              [bounds #:mutable #:auto]) ;rect
  #:transparent)

(define (node-origin node)
  (values (rect-x (node-bounds node))
          (rect-y (node-bounds node))))

(define (node-origin-x node)
  (rect-x (node-bounds node)))

(define (node-origin-y node)
  (rect-y (node-bounds node)))

(define (node-x-extent node)
  (+ (node-origin-x node)
     (node-width node)))

(define (node-y-extent node)
  (+ (node-origin-y node)
     (node-height node)))

(define (node-width node) 
  (rect-w (node-bounds node)))

(define (node-height node) 
  (rect-h (node-bounds node)))

;;control-point : node symbol symbol -> (values uint uint)
(define (control-point node horiz vert)
  (unless (node-bounds node)
    (error 'control-point "bounds undefined for node: ~a" node))
  (match-define (rect nx ny nw nh) (node-bounds node))
  (values (case horiz
            [(left) nx]
            [(center) (+ nx (/ nw 2))]
            [(right) (+ nx nw)])
          (case vert
            [(top) ny]
            [(center) (+ ny (/ nh 2))]
            [(bottom) (+ ny nh)])))

(struct edge (tail-node ;start node  (tail-node -----> head-node)
              head-node ;end node
              [view-drawer #:mutable]) ;(node node -> pict)
  #:transparent)

(struct interaction (event view-drawer))

(struct exec (start-time end-time all-futures))
(struct future (id real-time spawned-futures barricades syncs allocs) #:transparent)
(struct thread (id all-events))
(struct event (index start-time end-time previous-event next-event))

(define current-visualization (make-parameter #f))
(define current-visualization-data (make-parameter #f))
(define current-node-data (make-parameter #f))