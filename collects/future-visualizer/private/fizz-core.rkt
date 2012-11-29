#lang racket/base
(require (only-in racket/match match-define)
         (only-in racket/function curry identity)
         (only-in racket/list partition)
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
         (struct-out _line)
         (struct-out _interaction)
         (struct-out rect) ;Structure used for bounding-box data, etc.
         (struct-out _rectangle) ;The rectangle shape used for node viewing, etc.
         (struct-out _circle)
         get-size
         draw)

;Node is the supertype for all visual elements (primitives or compounds/views)
(struct _element ([parent #:mutable #:auto] ;(or _element #f)
                  [bounds #:mutable #:auto] ;rect
                  [pict #:mutable #:auto])) ;pict (cache for performance)
(struct _node _element (data
                        [children #:mutable]
                        [from-edges #:mutable]
                        [to-edges #:mutable]
                        back-color
                        fore-color
                        stroke-width
                        stroke-color
                        opacity
                        [interactions #:mutable #:auto]
                        [bounds #:mutable #:auto]) #:transparent) ;rect
(struct _edge _element (from to))
(struct _primedge _edge (color
                         opacity) #:transparent)
(struct _circle _node (diam))
(struct _rectangle _node (width height))
(struct _line _primedge ())
(struct _label _node (text))
  
(struct _view _node (margin scale-to-canvas? layout))

(define (get-size elem)
  (cond 
    [(_circle? elem) (values (_circle-diam elem) 
                             (_circle-diam elem))]
    [(_rectangle? elem) (values (_rectangle-width elem)
                                (_rectangle-height elem))]
    [(_label? elem) 
     (define tp (text (_label-text elem)))
     (values (pict-width tp)
             (pict-height tp))]
    [(_line? elem) 
     (define f (_edge-from elem))
     (define t (_edge-to elem))
     (define-values (fcx fcy) (control-point f 'center 'center))
     (define-values (tcx tcy) (control-point t 'center 'center))
     (values (abs (- fcx tcx))
             (abs (- fcy tcy)))]
    [(_view? elem) 
     ;Calculate layout
     ((_view-layout elem) elem)
     (define-values (mx my) 
       (let loop ([maxx 0] [maxy 0] [children (_node-children elem)])
         (cond 
           [(null? children) (values maxx maxy)]
           [(_edge? (car children)) (loop maxx maxy (cdr children))]
           [else 
            (define c (car children))
            (loop (max maxx (element-x-extent c))
                  (max maxy (element-y-extent c))
                  (cdr children))])))
     (values (+ mx (* (_view-margin elem) 2))
             (+ my (* (_view-margin elem) 2)))]))


;Need to update each element's bounds, including
;the root (which will just be the 'bounds' argument).
(define (draw elem [bounds #f])
  ;Translate the element's bounds into absolute coordinates
  (define mybounds (_element-bounds elem))
  (define parent (_element-parent elem))
  (if (and (not (_edge? elem)) parent)
    (set-_element-bounds! elem (rect (+ (element-origin-x parent) (rect-x mybounds))
                                (+ (element-origin-y parent) (rect-y mybounds))
                                (rect-w mybounds)
                                (rect-h mybounds)))
    (set-_element-bounds! elem bounds))
  (cond 
    [(_element-pict elem) ;If the element has a cached pict, use it
     (_element-pict elem)]
    [(_circle? elem)
     (colorize (disk (_circle-diam elem)) (_node-back-color elem))]
    [(_rectangle? elem)
     (colorize (filled-rectangle (_rectangle-width elem)
                                 (_rectangle-height elem))
               (_node-back-color elem))]
    [(_label? elem)
     (text (_label-text elem))]
    [(_line? elem)
     (define-values (fcx fcy) (control-point (_edge-from elem) 'center 'center))
     (define-values (tcx tcy) (control-point (_edge-to elem) 'center 'center))
     (set-_element-bounds! elem (rect fcx fcy (- fcx tcx) (- fcy tcy)))
     (colorize (pip-line (- fcx tcx)
                         (- tcy fcy)
                         0)
               (_primedge-color elem))]
    [(_view? elem) 
     ;Calculate layout (can we avoid doing this on each draw?)
     ;((_view-layout elem) elem)
     ;get-size will calculate layout for us
     (define-values (w h) (get-size elem))
     ;Draw each child and overlay     
     (define p 
       (cond 
         [(zero? (_node-stroke-width elem))
          (colorize (filled-rectangle w h) (_node-back-color elem))]
         [else 
          (define sw (_node-stroke-width elem))
          (define outer (colorize (filled-rectangle w h) (_node-stroke-color elem)))
          (cc-superimpose outer 
                          (colorize (filled-rectangle (- w (* sw 2))
                                                      (- h (* sw 2))) 
                                    (_node-back-color elem)))]))
     (define-values (edges nds) 
       (partition _edge? (_node-children elem)))
     (define ep (for/fold ([p p]) ([e (in-list edges)])
                  (define-values (fcx fcy) (control-point (_edge-from e)  
                                                          'center 
                                                          'center))
                  (pin-over p fcx fcy (draw e))))
     (for/fold ([p ep]) ([c (in-list nds)])
                  (pin-over p
                            (element-origin-x c)
                            (element-origin-y c)
                            (draw c)))]))

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