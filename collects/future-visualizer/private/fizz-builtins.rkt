#lang racket/base
(require (rename-in slideshow/pict 
                    [circle pict-circle]
                    [rectangle pict-rectangle]
                    [text pict-text])
         "fizz-syntax.rkt"
         "fizz-core.rkt")
(provide tree
         stack
         hierarchical-list
         elastic-timeline
         circle
         rect)

;BUILT-IN LAYOUT DRAWERS
;-----------------------
(define (tree/private parent margin x y mx my)
  (define node-view ((node-view-drawer parent) (node-data parent)))
  (define w (pict-width node-view))
  (define h (pict-height node-view))
  (cond 
    [(empty? (node-out-edges parent))
     (define nx (+ margin x))
     (define ny (+ margin y))
     (define xe (+ nx w))
     (define ye (+ ny h))
     (set-node-x! parent nx)
     (set-node-y! parent ny)
     (values (max mx xe)
             (max my ye))]
    [else
     (define child-y (+ y h))
     (define parenty (+ y margin))
     (define first-child (edge-head-node (first (node-out-edges parent))))
     (cond 
       [(= 1 (length (node-out-edges parent))) ;Align parent and child vertically
        (define-values (cmx cmy) (tree/private first-child
                                               margin
                                               x
                                               (+ parenty h)
                                               mx 
                                               my))
        (define-values (cx _) (control-point first-child 'center 'top))
        (define nx (max (- cx (/ w 2)) (+ x margin)))
        (define ny (+ padding y))
        (define xe (+ nx w))
        (define ye (+ ny h))
        (set-node-x! parent nx)
        (set-node-y! parent ny)
        (values (max cmx xe)
                (max cmy ye))]
       [else
        (define-values (cmx cmy)
          (for/fold ([xacc x] [yacc y]) ([child (in-list (map edge-head-node (node-out-edges parent)))])
            (define-values (cmx cmy) (tree/private child
                                                   margin
                                                   xacc
                                                   (+ parenty h)
                                                   mx
                                                   my))))
        (define xmin (node-x first-child))
        (define xmax cmx)
        (define nx (- (+ xmin (/ (- xmax xmin) 2))
                      (/ w 2)))
        (define ny (+ margin y))
        (define xe (max (+ nx w) cmx))
        (define ye (max (+ ny h) cmy))
        (set-node-x! parent nx)
        (set-node-y! parent ny)
        (values (max xe mx)
                (max ye my))])]))

(define (tree #:margin [margin 10]) 
  (λ (data nodes)
    (tree/private (findf (λ (n) (null? (node-in-edges n))) nodes)
                  margin
                  0 
                  0
                  0 
                  0))

(define (stack #:orientation [orientation 'vertical]
               #:margin [margin 0])
  (λ (data nodes)
    (case orientation
      [(vertical) 
       (apply vl-append 
              (cons margin 
                    (map (λ (n) (node-view-drawer n)) nodes)))]
      [(horizontal) 
       (apply htl-append
              (cons margin
                    (map (λ (n) (node-view-drawer n)) nodes)))])))

(define (hierarchical-list data nodes)
  0)

(define (elastic-timeline data nodes)
  0)

;BUILT-IN VIEWS
;--------------

;Circle view
(define-view circle (diameter color)
  #:layout-view (λ (vis-data nodes)
                  (colorize (filled-ellipse diameter diameter) color)))

;Rectangle view
;(define-view rect (width height color [text #f])  (optional arguments?)
(define-view rect (width height back-color fore-color text)
  #:layout-view 
  (λ (vis-data nodes)
    (cond 
      [text 
       (define t (colorize (pict-text (format "~a" text)) fore-color))
       (define r (colorize (filled-rectangle (if (auto? width)
                                                 (pict-width t)
                                                 width)
                                             (if (auto? height)
                                                 (pict-height t)
                                                 height))
                           back-color))
       (cc-superimpose r t)]
      [else
       (cond 
         [(or (auto? width) (auto? height))
          (error 'rect "Width/height cannot be auto if container is empty.")]
         [else
          (colorize (filled-rectangle width height) back-color)])])))