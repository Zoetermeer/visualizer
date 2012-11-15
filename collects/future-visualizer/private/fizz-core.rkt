#lang racket/base
(require (only-in racket/match match-define))
(provide current-visualization
         current-visualization-data
         current-node-data
         (struct-out view)
         (struct-out node)
         view-interaction-for
         view-width
         view-height
         view-origin-x
         view-origin-y
         view-x-extent
         view-y-extent
         control-point
         (struct-out edge)
         (struct-out interaction)
         (struct-out profile)
         (struct-out future)
         (struct-out thread)
         (struct-out event)
         (struct-out rect))

(struct rect (x y w h) #:transparent)
(struct view (name ;symbol
              data ;any
              parent ;(or view #f)  #f if the root view
              nodes ;(listof node)
              [layout-drawer #:mutable] ;(viewable-region -> pict)
              scale-to-canvas? ;bool
              [layout-pict #:mutable #:auto] ; (or pict #f)
              [bounds #:mutable #:auto] ;rect
              [hovered-node #:mutable #:auto] ; (or node #f), only needed for performance in mouseover
              [interactions #:mutable #:auto]) ;(or (listof interaction) #f)
  #:transparent)
(struct node (data 
              [in-edges #:mutable] ;(listof edge)
              [out-edges #:mutable] ;(listof edge)
              [view #:mutable #:auto]) ;view
  #:transparent)

;;view-interaction-for-type : symbol view -> interaction
(define (view-interaction-for type vw)
  (let loop ([ints (view-interactions vw)]) 
    (cond 
      [(null? ints) #f]
      [else 
       (define i (car ints))
       (if (equal? (interaction-type i) type)
           i
           (loop (cdr ints)))])))

(define (view-origin view)
  (values (rect-x (view-bounds view))
          (rect-y (view-bounds view))))

(define (view-origin-x view)
  (rect-x (view-bounds view)))

(define (view-origin-y view)
  (rect-y (view-bounds view)))

(define (view-x-extent view)
  (+ (view-origin-x view)
     (view-width view)))

(define (view-y-extent view)
  (+ (view-origin-y view)
     (view-height view)))

(define (view-width view) 
  (rect-w (view-bounds view)))

(define (view-height view) 
  (rect-h (view-bounds view)))

;type : symbol
;handler : (node viewable-region -> pict)
(struct interaction (type handler) #:transparent)

;;control-point : view symbol symbol -> (values uint uint)
(define (control-point view horiz vert)
  (unless (view-bounds view)
    (error 'control-point "bounds undefined for view: ~a" view))
  (match-define (rect nx ny nw nh) (view-bounds view))
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

(struct profile (start-time end-time all-futures))
(struct future (id real-time spawned-futures barricades syncs allocs) #:transparent)
(struct thread (id all-events))
(struct event (index start-time end-time previous-event next-event))

(define current-visualization (make-parameter #f))
(define current-visualization-data (make-parameter #f))
(define current-node-data (make-parameter #f))