#lang racket/base
(require "fizz-core.rkt")
(provide auto 
         stack
         tree
         circular)

(define (auto margin)
  50)

;Tree layout
;;set-tree-layout! : _node uint uint uint uint rect -> (values uint uint)
(define (set-tree-layout! parent padding x y mx my)
  (define (set-edge-bounds! edge parent child)
    (define-values (pcx pcy) (control-point parent 'center 'center))
    (define-values (ccx ccy) (control-point child 'center 'center))
    (set-_element-bounds! edge (rect (min pcx ccx) (min pcy ccy) (abs (- pcx ccx)) (abs (- pcy ccy)))))    
  ;Get the node's dimensions
  (define-values (w h) (get-size parent))
  (cond 
    [(or (not (_node-from-edges parent))
         (null? (_node-from-edges parent)))
     (define nx (+ padding x))
     (define ny (+ padding y))
     (define xe (+ nx w))
     (define ye (+ ny h))
     (set-_element-bounds! parent (rect nx ny w h))
     (values (max mx xe)
             (max my ye))]
    [else
     (define child-y (+ y h))
     (define parenty (+ y padding))
     (define fc-edge (car (_node-from-edges parent)))
     (define first-child (_edge-to fc-edge))
     (cond 
       [(= 1 (length (_node-from-edges parent))) ;Align parent and child vertically
        (define-values (cmx cmy) (set-tree-layout! first-child
                                                   padding
                                                   x
                                                   (+ parenty h)
                                                   mx 
                                                   my))
        (define-values (cx _) (control-point first-child 'center 'top))
        (define nx (max (- cx (/ w 2)) (+ x padding)))
        (define ny (+ padding y))
        (define xe (+ nx w))
        (define ye (+ ny h))
        (set-_element-bounds! parent (rect nx ny w h))
        (set-edge-bounds! fc-edge parent first-child)
        (values (max cmx xe)
                (max cmy ye))]
       [else
        (define-values (cmx cmy)
          (for/fold ([xacc x] [yacc y]) ([cedge (in-list (_node-from-edges parent))])
            (define child (_edge-to cedge))
            (define-values (cmx cmy) (set-tree-layout! child
                                                       padding
                                                       xacc
                                                       (+ parenty h)
                                                       mx
                                                       my))
            (values (max xacc cmx) (max yacc cmy))))
        (define xmin (element-origin-x first-child))
        (define xmax cmx)
        (define nx (- (+ xmin (/ (- xmax xmin) 2))
                      (/ w 2)))
        (define ny (+ padding y))
        (define xe (max (+ nx w) cmx))
        (define ye (max (+ ny h) cmy))
        (set-_element-bounds! parent (rect nx ny w h))
        (values (max xe mx)
                (max ye my))])]))

;Is just the node argument enough?
(define (tree #:padding [padding 10])
  (λ (node) 
    (define data (_node-data node))
    (define nodes (_node-children node))
    (define roots (filter (λ (n) (and (_node? n) (null? (_node-to-edges n)))) nodes))
    (when (null? roots)
      (error 'tree "expected a tree or collection of trees but got ~a in: ~a." roots node))
    (let loop ([maxx 0] 
               [maxy 0]
               [rts roots])
      (cond 
        [(null? rts) (void)]
        [else
         (define r (car rts))
         (define-values (mx my) 
           (set-tree-layout! r 
                             padding
                             maxx
                             0
                             maxx 
                             maxy))
         (loop (max maxx mx)
               (max maxy my)
               (cdr rts))]))))

(define (stack #:orientation [orientation 'horizontal] #:padding [padding 10])
  (λ (node)
    (let loop ([∆x (+ 0 padding)]
               [∆y (+ 0 padding)]
               [children (_node-children node)])
      (unless (null? children)
        (define child (car children))
        (define-values (cw ch) (get-size child))
        (set-_element-bounds! child (rect ∆x ∆y cw ch))
        (loop (+ ∆x cw padding)
              ∆y 
              (cdr children))))))

(define (circular)
  (λ (node)
    ;Construct a list where the ordering matches the order in which 
    ;nodes will be drawn around the circle. 
    ;This layout uses the Baur and Brandes algorithm, which applies a 
    ;heuristic to reduce the number of crossings
    (void)))
