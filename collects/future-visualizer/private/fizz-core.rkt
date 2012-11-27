#lang racket/base
(require (only-in racket/match match-define)
         (only-in racket/function curry identity)
         (rename-in slideshow/pict
                    [circle pict-circle]
                    [rectangle pict-rectangle]))
(provide element-width
         element-height
         element-origin-x
         element-origin-y
         element-x-extent
         element-y-extent
         control-point
         (struct-out _element)
         (struct-out _node)
         (struct-out _view)
         (struct-out _edge)
         (struct-out _interaction)
         (struct-out rect)
         view
         nodes
         edges-from
         layout
         circle
         rectangle
         line
         tree
         stack
         draw)

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

(define (line from to)
  (_line from to))

;Tree layout
;;set-tree-layout! : _node uint uint uint uint rect -> (values uint uint)
(define (set-tree-layout! parent margin x y mx my bounds)
  ;Draw the node to get its dimensions, then cache
  (define pct (draw parent bounds))
  (set-_element-pict! parent pct)
  (define w (pict-width pct))
  (define h (pict-height pct))
  (cond 
    [(or (not (_node-from-edges parent))
         (null? (_node-from-edges parent)))
     (define nx (+ margin x))
     (define ny (+ margin y))
     (define xe (+ nx w))
     (define ye (+ ny h))
     (set-_element-bounds! parent (rect nx ny w h))
     (values (max mx xe)
             (max my ye))]
    [else
     (define child-y (+ y h))
     (define parenty (+ y margin))
     (define first-child (_edge-to (car (_node-from-edges parent))))
     (cond 
       [(= 1 (length (_node-from-edges parent))) ;Align parent and child vertically
        (define-values (cmx cmy) (set-tree-layout! first-child
                                                   margin
                                                   x
                                                   (+ parenty h)
                                                   mx 
                                                   my
                                                   bounds))
        (define-values (cx _) (control-point first-child 'center 'top))
        (define nx (max (- cx (/ w 2)) (+ x margin)))
        (define ny (+ margin y))
        (define xe (+ nx w))
        (define ye (+ ny h))
        (set-_element-bounds! parent (rect nx ny w h))
        (values (max cmx xe)
                (max cmy ye))]
       [else
        (define-values (cmx cmy)
          (for/fold ([xacc x] [yacc y]) ([child (in-list (map _edge-to (_node-from-edges parent)))])
            (define-values (cmx cmy) (set-tree-layout! child
                                                       margin
                                                       xacc
                                                       (+ parenty h)
                                                       mx
                                                       my
                                                       bounds))
            (values (max xacc cmx) (max yacc cmy))))
        (define xmin (element-origin-x first-child))
        (define xmax cmx)
        (define nx (- (+ xmin (/ (- xmax xmin) 2))
                      (/ w 2)))
        (define ny (+ margin y))
        (define xe (max (+ nx w) cmx))
        (define ye (max (+ ny h) cmy))
        (set-_element-bounds! parent (rect nx ny w h))
        (values (max xe mx)
                (max ye my))])]))

;Is just the node argument enough?
(define (tree #:margin [margin 10])
  (λ (node bounds) 
    (define data (_node-data node))
    (define nodes (_node-children node))
    (define roots (filter (λ (n) (or (not (_node-to-edges n))
                                     (null? (_node-to-edges n)))) nodes))
    (when (null? roots)
      (error 'tree "expected a tree or collection of trees but got ~a in: ~a." roots node))
    (let loop ([maxx (rect-x bounds)] 
               [maxy (rect-y bounds)]
               [rts roots])
      (cond 
        [(null? rts) (void)]
        [else
         (define r (car rts))
         (define-values (mx my) 
           (set-tree-layout! r 
                             margin
                             maxx
                             (rect-y bounds)
                             maxx 
                             maxy
                             bounds))
         (loop (max maxx mx)
               (max maxy my)
               (cdr rts))]))))

(define (stack #:orientation [orientation 'horizontal] #:margin [margin 10])
  (λ (node bounds)
    0))

;Node is the supertype for all visual elements (primitives or compounds/views)
(struct _element ([parent #:mutable #:auto] ;(or _element #f)
                  [bounds #:mutable #:auto] ;rect
                  [pict #:mutable #:auto])) ;pict (cache for performance)
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
(struct _line _primedge ())
(struct _label _primnode (text))
  
(struct _view _node (scale-to-canvas? layout))

;Need to update each element's bounds, including
;the root (which will just be the 'bounds' argument).
(define (draw elem [bounds (rect 0 0 1000 1000)]) 
  (printf "draw ~a with bounds: ~a\n" elem (_element-bounds elem))
  (cond 
    [(_element-pict elem) ;If the element has a cached pict, use it
     (_element-pict elem)]
    [(_circle? elem)
     (colorize (disk (_circle-diam elem)) (_primnode-back-color elem))]
    [(_rectangle? elem)
     (colorize (filled-rectangle (_rectangle-width elem)
                                 (_rectangle-height elem))
               (_primnode-back-color elem))]
    [(_label? elem)
     (text (_label-text elem))]
    [(_line? elem)
     (blank 10 10)] ;TODO: Fix!  How do we get 'dx' and 'dy'?
    [(_view? elem) 
     ;Calculate layout (can we avoid doing this on each draw?)
     ;Draw each child
     ((_view-layout elem) elem bounds)
     (define p (blank (rect-w bounds) (rect-h bounds)))
     (for/fold ([p p]) ([c (in-list (_node-children elem))])
       (define b (_element-bounds c))
       (pin-over p
                 (element-origin-x c)
                 (element-origin-y c)
                 (draw c (_element-bounds c))))]))


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

(define (edges-from get-to-values
                    #:shape [shape _line])
  (λ (from-node all-nodes)
    (define tos (get-to-values (_node-data from-node)))
    (define to-nodes (find-nodes tos all-nodes))
    (for/list ([tn (in-list to-nodes)])
      (define e (shape from-node tn))
      (set-_element-parent! e (_element-parent from-node))
      (set-_node-to-edges! tn (cons e (_node-to-edges tn)))
      (set-_node-from-edges! from-node (cons e (_node-from-edges from-node)))
      e)))

;Apply lt if a function with 0 arity, 
;so we can say (layout tree) instead 
;of (layout (tree)). 
(define (layout lt)
  (cond 
    [(zero? (procedure-arity lt)) (lt)]
    [else lt]))

(define (view nds  
              #:edges [edge-getter #f] 
              #:scale-to-bounds [scale-to-canvas? #f]
              [layout tree] 
              . interactions)
  (λ (data)
    (define vw (_view data '() scale-to-canvas? layout))
    (define root-nodes (nds data))
    (for ([n (in-list root-nodes)])
      (set-_element-parent! n vw))
    (set-_node-children! vw root-nodes)
    (when edge-getter
      (for ([n (in-list root-nodes)])
        (edge-getter n root-nodes)))
    vw))

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

(define (element-origin element)
  (values (rect-x (_element-bounds element))
          (rect-y (_element-bounds element))))

(define (element-origin-x element)
  (rect-x (_element-bounds element)))

(define (element-origin-y element)
  (rect-y (_element-bounds element)))

(define (element-x-extent element)
  (+ (element-origin-x element)
     (element-width element)))

(define (element-y-extent element)
  (+ (element-origin-y element)
     (element-height element)))

(define (element-width element) 
  (rect-w (_element-bounds element)))

(define (element-height element) 
  (rect-h (_element-bounds element)))

;;control-point : _element symbol symbol -> (values uint uint)
(define (control-point element horiz vert)
  (unless (_element-bounds element)
    (error 'control-point "bounds undefined for element ~a" element))
  (match-define (rect nx ny nw nh) (_element-bounds element))
  (values (case horiz
            [(left) nx]
            [(center) (+ nx (/ nw 2))]
            [(right) (+ nx nw)])
          (case vert
            [(top) ny]
            [(center) (+ ny (/ nh 2))]
            [(bottom) (+ ny nh)])))

(define (auto margin)
  50)