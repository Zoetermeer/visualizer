#lang racket/base
(require (only-in racket/match match-define)
         (only-in racket/function curry identity))
(provide current-visualization
         current-visualization-data
         current-node-data
         (struct-out view)
         construct-view
         (struct-out node)
         view-interaction-for
         view-width
         view-height
         view-origin-x
         view-origin-y
         view-x-extent
         view-y-extent
         control-point
         build-view
         (struct-out edge)
         (struct-out interaction)
         (struct-out profile)
         (struct-out future)
         (struct-out thread)
         (struct-out event)
         (struct-out rect))

(struct rect (x y w h) #:transparent)

(define (construct-view name data parent nodes [layout-drawer #f] [scale-to-canvas? #f])
  (define v (view name data parent nodes layout-drawer scale-to-canvas? '()))
  (when parent
    (set-view-children! parent (cons v (view-children parent))))
  v)
  
(struct view (name ;symbol
              data ;any
              parent ;(or view #f)  #f if the root view
              nodes ;(listof node)
              [layout-drawer #:mutable] ;(viewable-region -> pict)
              scale-to-canvas? ;bool
              [children #:mutable]
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
(struct interaction (type handler [view #:mutable #:auto]) #:transparent)

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
              view) ;pict
  #:transparent)

;Find all nodes n (from nodes) for which (node-data n) is equal 
;to some element of vs.
;;find-nodes : (listof any) (listof node) -> (listof node)
(define (find-nodes vs nodes) 
  (define ns (map (λ (v) (findf (λ (n) (equal? (node-data n) v)) nodes)) vs))
  (filter identity ns))

(define (build-view name 
                    #:nodes [nodes (λ (data) '())]
                    #:out-edges [out-edges (λ (node-data) '())]
                    #:node-view [node-view-builder #f]
                    #:edge-view [edge-view-builder #f]
                    #:scale-to-canvas? [scale-to-canvas? #f]
                    #:layout layout
                    . interactions)
  (λ (parent-view data)
    ;Each invocation needs its own copies of interactions
    ;Would be nicer to use struct-copy here, but it seems to choke
    ;when a struct has #:auto fields
    (define my-inters (map (λ (i) (interaction (interaction-type i)
                                               (interaction-handler i))) interactions))
    (define nds (map (λ (v) (node v '() '())) (nodes data)))
    (when (and (not (null? nds)) (not node-view-builder))
      (error 'build-view "view with nodes must have an associated node view"))
    (define vw (construct-view name data parent-view nds #f scale-to-canvas?))
    (for ([n (in-list nds)])
      (define ndata (node-data n))
      (set-node-view! n (node-view-builder vw ndata))
      (define outs (out-edges ndata))
      (define out-nodes (find-nodes (out-edges ndata) nds))
      (for ([o-n (in-list out-nodes)])
        (define e (edge n o-n (edge-view-builder vw n o-n)))
        (set-node-out-edges! n (cons e (node-out-edges n)))
        (set-node-in-edges! o-n (cons e (node-in-edges o-n)))))
    (set-view-layout-drawer! vw ((curry layout) vw))
    ;Evaluate each interaction's view builder and set its parent
    (for ([int (in-list my-inters)])
      (set-interaction-view! int ((interaction-handler int) vw data)))
    (set-view-interactions! vw my-inters)
    vw))

(struct profile (start-time end-time all-futures))
(struct future (id real-time spawned-futures barricades syncs allocs) #:transparent)
(struct thread (id all-events))
(struct event (index start-time end-time previous-event next-event))

(define current-visualization (make-parameter #f))
(define current-visualization-data (make-parameter #f))
(define current-node-data (make-parameter #f))