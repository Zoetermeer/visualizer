#lang racket/base
(require (only-in racket/match match-define)
         (only-in racket/function curry identity))
(provide node-width
         node-height
         node-origin-x
         node-origin-y
         node-x-extent
         node-y-extent
         control-point
         (struct-out _node)
         (struct-out _view)
         (struct-out _interaction)
         (struct-out rect)
         view
         nodes
         circle
         rectangle
         tree)

(define (circle #:diameter [diameter (auto 10)]
                #:back-color [back-color "blue"]
                #:fore-color [fore-color "white"]
                #:stroke-width [stroke-width 0]
                #:stroke-color [stroke-color "black"]
                #:opacity [opacity 1])
  (λ (data)
    (_circle data 
             '()
             back-color 
             fore-color 
             stroke-width 
             stroke-color
             opacity
             diameter)))

(define (rectangle #:width [width (auto 20)]
                   #:height [height (auto 20)]
                   #:back-color [back-color "blue"]
                   #:fore-color [fore-color "white"]
                   #:stroke-width [stroke-width 0]
                   #:stroke-color [stroke-color "black"]
                   #:opacity [opacity 1])
  (λ (data)
    (_rectangle data
                '()
                back-color
                fore-color
                stroke-width
                stroke-color
                opacity
                width
                height)))

;Tree layout
;Is just the node argument enough?
(define (tree #:margin [margin 10])
  (λ (node) 
    0))

;Node is the supertype for all visual elements (primitives or compounds/views)
(struct _element ([parent #:mutable #:auto] [bounds #:mutable #:auto]))
(struct _node _element (data
                        [children #:mutable]
                        [from-edges #:mutable #:auto]
                        [to-edges #:mutable #:auto]
                        [interactions #:mutable #:auto]
                        [bounds #:mutable #:auto]) #:transparent) ;rect
(struct _edge _element (from to))
(struct _primnode _node (back-color
                          fore-color
                          stroke-width
                          stroke-color
                          opacity) #:transparent)
(struct _primedge _edge (color
                         opacity) #:transparent)
(struct _circle _primnode (diam))
(struct _rectangle _primnode (width height))
(struct _line _primedge (from to))
(struct _label _primnode (text))
  
(struct _view _node (scale-to-canvas? layout))


(define (nodes get-node-values
               #:shape [shape (rectangle #:width 50 #:height 50)]
               #:foreach [foreach #f])
  (λ (data)
    (define rootvs (get-node-values data))
    (define roots (map shape rootvs)) ;'shape' is a function taking a data arg
    (when foreach 
      (for ([v (in-list rootvs)]
            [n (in-list roots)])
        ;Expect the 'foreach' expression to be (usually)
        ;the result of calling `nodes', so a function taking a
        ;data argument
        (define children (foreach v))
        (set-_node-children! n children)
        (for ([cn (in-list children)])
          (set-_element-parent! cn n))))
    roots))

(define (edges-to get-from-values
                  #:shape [shape _line])
  (λ (to-node nodes)
    (define froms (get-from-values (_node-data to-node)))
    (define from-nodes (find-nodes froms nodes))
    (for ([fn (in-list from-nodes)])
      (set-_node-to-edges! (
    (for ([edge (in-list ->s)])
          (set-_node-to-edges! (cons edge (_node-to-edges n)))
          (set-_node-from-edges! (cons edge (_node-from-edges to-node))))
    
    (map (λ (fn) 
           (shape fn to-node))
         from-nodes)))

#|
(view 
  (nodes ...)
  (edges-from ...)
  (edges-to ...)
  (layout ...))
|#
(define (view nds 
              [egto #f] 
              [egfrom #f] 
              [scale-to-canvas? #f]
              [layout tree])
  (λ (data)
    (define root-nodes (nds data))
    (when egto
      (for ([n (in-list root-nodes)])
        (define ->s (egto n root-nodes))
        (for ([edge (in-list ->s)])
          (set-_node-to-edges! (cons edge (_node-to-edges n)))
          (set-_node-from-edges! (cons edge (_node-from-edges to-node))))))
    (when egfrom
      (for ([n (in-list root-nodes)])
        (define <-s (egfrom n root-nodes))
        (for ([edge (in-list <-s)])
          (set-_node-to-edges! (cons edge (_node-to-edges from-node)))
          (set-_node-from-edges! (cons edge (_node-from-edges n))))))
    (_view scale-to-canvas? layout)))

;Find all nodes n (from nodes) for which (node-data n) is equal 
;to some element of vs.
;;find-nodes : (listof any) (listof node) -> (listof node)
(define (find-nodes vs nodes) 
  (define ns (map (λ (v) (findf (λ (n) (equal? (_node-data n) v)) nodes)) vs))
  (filter identity ns)) 


(struct rect (x y w h) #:transparent)
  
(struct _interaction ())
(struct hover _interaction (shape))
(struct highlight-when-over _interaction ())

(define (node-origin node)
  (values (rect-x (_node-bounds node))
          (rect-y (_node-bounds node))))

(define (node-origin-x node)
  (rect-x (_node-bounds node)))

(define (node-origin-y node)
  (rect-y (_node-bounds node)))

(define (node-x-extent node)
  (+ (node-origin-x node)
     (node-width node)))

(define (node-y-extent node)
  (+ (node-origin-y node)
     (node-height node)))

(define (node-width node) 
  (rect-w (_node-bounds node)))

(define (node-height node) 
  (rect-h (_node-bounds node)))

;;control-point : _node symbol symbol -> (values uint uint)
(define (control-point node horiz vert)
  (unless (_node-bounds node)
    (error 'control-point "bounds undefined for node ~a" node))
  (match-define (rect nx ny nw nh) (_node-bounds node))
  (values (case horiz
            [(left) nx]
            [(center) (+ nx (/ nw 2))]
            [(right) (+ nx nw)])
          (case vert
            [(top) ny]
            [(center) (+ ny (/ nh 2))]
            [(bottom) (+ ny nh)])))

(define (auto margin)
  0)