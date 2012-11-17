#lang racket/base
(require (rename-in slideshow/pict 
                    [circle pict-circle]
                    [rectangle pict-rectangle]
                    [text pict-text])
         (only-in racket/match match match-define)
         "fizz-core.rkt"
         "display.rkt")
(provide tree
         stack
         hierarchical-list
         elastic-timeline
         menubar
         box-at
         circle
         rectangle
         edge-line
         (struct-out auto))

(struct auto (margin) #:transparent)

;;auto? : any -> boolean
(define (is-auto? v)
  (match v
    [(auto _) #t]
    ['auto #t]
    [else #f]))

(define (abs-or-auto-for child get-size want-size)
  (if (is-auto? want-size)
      (+ (get-size child) 
         (* 2 (auto-margin want-size)))
      want-size))

;BUILT-IN LAYOUT DRAWERS
;-----------------------
(define (set-tree-layout! parent margin x y mx my vregion)
  (define ndview (node-view parent))
  (define pct ((view-layout-drawer ndview) vregion))
  (set-view-layout-pict! ndview pct)
  (define w (pict-width pct))
  (define h (pict-height pct))
  (cond 
    [(null? (node-out-edges parent))
     (define nx (+ margin x))
     (define ny (+ margin y))
     (define xe (+ nx w))
     (define ye (+ ny h))
     (set-view-bounds! ndview (rect nx ny w h))
     (values (max mx xe)
             (max my ye))]
    [else
     (define child-y (+ y h))
     (define parenty (+ y margin))
     (define first-child (edge-head-node (car (node-out-edges parent))))
     (cond 
       [(= 1 (length (node-out-edges parent))) ;Align parent and child vertically
        (define-values (cmx cmy) (set-tree-layout! first-child
                                                   margin
                                                   x
                                                   (+ parenty h)
                                                   mx 
                                                   my
                                                   vregion))
        (define-values (cx _) (control-point (node-view first-child) 'center 'top))
        (define nx (max (- cx (/ w 2)) (+ x margin)))
        (define ny (+ margin y))
        (define xe (+ nx w))
        (define ye (+ ny h))
        (set-view-bounds! ndview (rect nx ny w h))
        (values (max cmx xe)
                (max cmy ye))]
       [else
        (define-values (cmx cmy)
          (for/fold ([xacc x] [yacc y]) ([child (in-list (map edge-head-node (node-out-edges parent)))])
            (define-values (cmx cmy) (set-tree-layout! child
                                                       margin
                                                       xacc
                                                       (+ parenty h)
                                                       mx
                                                       my
                                                       vregion))
            (values (max xacc cmx) (max yacc cmy))))
        (define xmin (view-origin-x (node-view first-child)))
        (define xmax cmx)
        (define nx (- (+ xmin (/ (- xmax xmin) 2))
                      (/ w 2)))
        (define ny (+ margin y))
        (define xe (max (+ nx w) cmx))
        (define ye (max (+ ny h) cmy))
        (set-view-bounds! ndview (rect nx ny w h))
        (values (max xe mx)
                (max ye my))])]))

(define (tree #:margin [margin 10]) 
  (λ (vw vregion)
    (define bounds (cond 
                     [(view-parent vw)
                      (view-bounds (view-parent vw))]
                     [else
                      (set-view-bounds! vw (rect (viewable-region-x vregion)
                                                 (viewable-region-y vregion)
                                                 (viewable-region-width vregion)
                                                 (viewable-region-height vregion)))
                      (view-bounds vw)]))
    (define data (view-data vw))
    (define nodes (view-nodes vw))
    (define roots (filter (λ (n) (null? (node-in-edges n))) nodes))
    (when (null? roots)
      (error 'tree "No root nodes found!"))
    (define-values (maxx maxy)
      (for/fold ([maxx (rect-x bounds)] [maxy (rect-y bounds)]) ([r (in-list roots)])
        (define-values (mx my) 
          (set-tree-layout! r
                            margin
                            maxx 
                            (rect-y bounds)
                            maxx 
                            maxy
                            vregion))
        (values (max maxx mx)
                (max maxy my))))    
    ;Draw three picts: 
    ;1) Blank with nodes superimposed
    ;2) Blank with edges superimposed
    ;3) Background pict (colored square or whatever)
    ;Then overlay 1 -> 2 -> 3
    (define-values (np ep) (for/fold ([np (blank maxx maxy)]
                                      [ep (blank maxx maxy)]) ([n (in-list nodes)])
                             (define nview (node-view n))
                             (define-values (ncx ncy) (control-point nview 'center 'center))
                             (values (pin-over np 
                                               (view-origin-x nview)
                                               (view-origin-y nview)
                                               (view-layout-pict nview))
                                     (for/fold ([out-ep ep]) ([ed (in-list (node-out-edges n))])
                                       (define edge-pict ((view-layout-drawer (edge-view ed)) vregion))
                                       (set-view-bounds! (edge-view ed) (rect ncx 
                                                                              ncy 
                                                                              (pict-width edge-pict) 
                                                                              (pict-height edge-pict)))
                                       (pin-over out-ep
                                                 ncx
                                                 ncy
                                                 edge-pict)))))
    (pin-over (pin-over ep 0 0 np)
              0
              0
              (blank maxx maxy))))    

(define (stack #:orientation [orientation 'vertical]
               #:margin [margin 0])
  (λ (vw vregion)
    (define nodes (view-nodes vw))
    (for ([n (in-list nodes)])
      (set-view-layout-pict! (node-view n) 
                             ((view-layout-drawer (node-view n)) vregion)))
    (define-values (pct _) (for/fold ([pct (blank (viewable-region-width vregion)
                                                  (viewable-region-height vregion))]
                                      [∆ margin]) ([n (in-list nodes)])
                             (define p (view-layout-pict (node-view n)))
                             (define-values (x y incf) (case orientation
                                                         [(vertical) (values margin ∆ pict-height)]
                                                         [(horizontal) (values ∆ margin pict-width)]))
                             (set-view-bounds! (node-view n) 
                                               (rect x y (pict-width p) (pict-height p)))                             
                             (values (pin-over pct 
                                               x
                                               y
                                               p)
                                     (+ ∆ (incf p) margin))))
    pct))

(define (hierarchical-list vw vregion) 0)

(define (elastic-timeline vw vregion)
    0)

(define (menubar #:margin [margin 10] #:items items)
  (λ (vw vregion)
      (define menu (apply hc-append 
                          (cons margin
                                (map (λ (i) (pict-text (format "~a" i))) items))))
      (pin-over (blank (viewable-region-width vregion)
                       (viewable-region-height vregion))
                0
                0
                menu)))

(define (box-at x y width height color vregion)
  (define p (blank (viewable-region-width vregion)
                   (viewable-region-height vregion)))
  (pin-over p 
            x
            y
            (colorize (filled-rectangle width height) color)))

(define (properties-get data . vs)
  (map (λ (v)
         (cond 
           [(and (procedure? v) (= (procedure-arity v) 1))
            (v data)]
           [else v]))
       vs))
    
;BUILT-IN VIEWS
;--------------
;Circle view 
(define (circle #:diameter diameter
                #:back-color back-color
                #:fore-color [fore-color "black"]
                #:stroke-width [stroke-width 0]
                #:stroke-color [stroke-color "black"]
                #:text [text #f]
                . interactions) 
  (apply build-view 
         'circle
         interactions
         #:layout (λ (vw vregion)
                         (match-define `(,diam ,bc ,fc ,stw ,stc ,txt) 
                           (properties-get (view-data vw) diameter
                                                          back-color
                                                          fore-color
                                                          stroke-width
                                                          stroke-color
                                                          text))
                         (cond 
                           [txt
                            (define t (colorize (pict-text (format "~a" txt)) fc))
                            (define the-diam (abs-or-auto-for t pict-width diam))
                            (define c (colorize (disk the-diam) bc))
                            (cc-superimpose c t)]
                           [else
                            (colorize (disk diam) bc)]))))


;Rectangle view
(define (rectangle #:width width
                   #:height height
                   #:back-color back-color
                   #:fore-color [fore-color "black"]
                   #:stroke-thickness [stroke-thickness 0]
                   #:stroke-color [stroke-color "black"]
                   #:text [text #f])
  (build-view 'rectangle
              #:layout
              (λ (vw vregion)
                  (define (draw-rect w h strokew strokec)
                    (cond 
                      [(zero? strokew)
                       (colorize (filled-rectangle w h) back-color)]
                      [else
                       (define inner (colorize (filled-rectangle (- w (* strokew 2)) 
                                                                 (- h (* strokew 2)))
                                               back-color))
                       (define outer (colorize (filled-rectangle w h) strokec))
                       (cc-superimpose outer inner)]))                  
                  (cond 
                    [text 
                     (define t (colorize (pict-text (format "~a" text)) fore-color))
                     (define r (draw-rect (abs-or-auto-for t pict-width width)
                                          (abs-or-auto-for t pict-height height)
                                          stroke-thickness
                                          stroke-color))
                     (cc-superimpose r t)]
                    [else
                     (cond 
                       [(or (auto? width) (auto? height))
                        (error 'rect "Width/height cannot be auto if container is empty.")]
                       [else
                        (draw-rect width height stroke-thickness stroke-color)])]))))

;Convenience view for edge lines in graphs
(define (edge-line #:style [style 'solid]
                   #:width [width 1.0]
                   #:color [color "black"])
  (λ (parent-view tail head)
    (define vw (build-view 'edge-line
                           #:layout
                           (λ (vw vregion)
                               (define-values (tcx tcy) (control-point (node-view tail) 'center 'center))
                               (define-values (hcx hcy) (control-point (node-view head) 'center 'center))
                               (define dx (- hcx tcx))
                               (define dy (- hcy tcy))
                               (linewidth width
                                          (linestyle style
                                                     (colorize (pip-line dx dy 0)
                                                               color))))))
    (vw parent-view #f)))