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
         rectangle)

;BUILT-IN LAYOUT DRAWERS
;-----------------------
(define (set-tree-layout! parent margin x y mx my vregion)
  (set-node-view-pict! parent ((node-view-drawer parent))) 
  (define w (pict-width (node-view-pict parent)))
  (define h (pict-height (node-view-pict parent)))
  (cond 
    [(null? (node-out-edges parent))
     (define nx (+ margin x))
     (define ny (+ margin y))
     (define xe (+ nx w))
     (define ye (+ ny h))
     (set-node-bounds! parent (rect nx ny w h))
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
        (define-values (cx _) (control-point first-child 'center 'top))
        (define nx (max (- cx (/ w 2)) (+ x margin)))
        (define ny (+ margin y))
        (define xe (+ nx w))
        (define ye (+ ny h))
        (set-node-bounds! parent (rect nx ny w h))
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
        (define xmin (node-origin-x first-child))
        (define xmax cmx)
        (define nx (- (+ xmin (/ (- xmax xmin) 2))
                      (/ w 2)))
        (define ny (+ margin y))
        (define xe (max (+ nx w) cmx))
        (define ye (max (+ ny h) cmy))
        (set-node-bounds! parent (rect nx ny w h))
        (values (max xe mx)
                (max ye my))])]))

(define (tree #:margin margin) 
  (λ (data nodes vregion)
    (define roots (filter (λ (n) (null? (node-in-edges n))) nodes))
    (define-values (maxx maxy)
      (for/fold ([maxx 0] [maxy 0]) ([r (in-list roots)])
        (define-values (mx my) 
          (set-tree-layout! r
                            margin
                            maxx 
                            0
                            maxx 
                            0
                            vregion))
        (values (max maxx mx)
                (max maxy my))))
    ;;REMOVEME
    (for ([n (in-list nodes)])
      (printf "Node: ~a, out edges=~a, in edges=~a, bounds=~a\n" 
              (node-data n)
              (length (node-out-edges n))
              (length (node-in-edges n))
              (node-bounds n)))
    
    ;Draw three picts: 
    ;1) Blank with nodes superimposed
    ;2) Blank with edges superimposed
    ;3) Background pict (colored square or whatever)
    ;Then overlay 1 -> 2 -> 3
    (define-values (np ep) (for/fold ([np (blank maxx maxy)]
                                     [ep (blank maxx maxy)]) ([n (in-list nodes)])
                            (define-values (ncx ncy) (control-point n 'center 'center))
                            (values (pin-over np 
                                              (node-origin-x n)
                                              (node-origin-y n)
                                              (node-view-pict n))
                                    (for/fold ([out-ep ep]) ([ed (in-list (node-out-edges n))])
                                      (pin-over out-ep
                                                ncx
                                                ncy
                                                ((edge-view-drawer ed)))))))
    (pin-over (pin-over ep 0 0 np)
              0
              0
              (blank maxx maxy))))
                           
    

(define (stack #:orientation [orientation 'vertical]
               #:margin [margin 0])
  (λ (data nodes)
    (case orientation
      [(vertical) 
       (apply vl-append 
              (cons margin 
                    (map (λ (n) (node-view-pict n)) nodes)))]
      [(horizontal) 
       (apply htl-append
              (cons margin
                    (map (λ (n) (node-view-pict n)) nodes)))])))

(define (hierarchical-list data nodes)
  0)

(define (elastic-timeline data nodes)
  0)

;BUILT-IN VIEWS
;--------------

;Circle view 
(define-view circle (diameter color)
  #:layout-view (λ (vis-data nodes vregion)
                  (colorize (filled-ellipse diameter diameter) color)))

;Rectangle view
;(define-view rect (width height color [text #f])  (optional arguments?)
(define-view rectangle (width height back-color fore-color text)
  #:layout-view 
  (λ (vis-data nodes vregion)
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