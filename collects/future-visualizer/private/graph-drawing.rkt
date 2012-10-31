#lang racket/base

(require racket/list
         racket/contract
         (only-in racket/match match-define)
         "constants.rkt")
(provide (struct-out node) 
         (struct-out drawable-node) 
         (struct-out graph-layout) 
         (struct-out attributed-node)
         draw-tree
         control-point
         flatten-tree
         build-attr-tree)

(struct node (data children))
(struct graph-layout (width height nodes) #:transparent)
(struct drawable-node (node x y width height depth children children-xextent children-yextent) #:transparent)

(define (flatten-tree/private dnodes)
  (cond
    [(empty? dnodes) '()]
    [else 
     (append dnodes 
             (for/list ([dn (in-list dnodes)])
               (flatten-tree/private (drawable-node-children dn))))]))

(define (flatten-tree dnodes)
  (flatten (flatten-tree/private dnodes)))

;;control-point : drawable-node (or 'left 'center 'right) (or 'top 'center 'bottom)
;;              -> (values uint uint)
(define (control-point dnode horiz vert)
  (define nx (drawable-node-x dnode))
  (define nw (drawable-node-width dnode))
  (define ny (drawable-node-y dnode))
  (define nh (drawable-node-height dnode))
  (values (case horiz
            [(left) nx]
            [(center) (+ nx (/ nw 2))]
            [(right) (+ nx nw)])
          (case vert
            [(top) ny]
            [(center) (+ ny (/ nh 2))]
            [(bottom) (+ ny nh)])))

(struct attributed-node (node type num-leaves depth children))

;;leaf? : attributed-node -> bool
(define (leaf? anode) 
  (equal? (attributed-node-type anode) 'leaf))

;;build-attr-tree : node uint -> attributed-node
(define (build-attr-tree parent depth) 
  (if (empty? (node-children parent)) 
      (attributed-node parent 'leaf 0 depth '()) 
      (let-values ([(leaves achn) 
                   (for/fold ([l 0] [achildren '()]) ([child (in-list (node-children parent))]) 
                     (let ([anode (build-attr-tree child (add1 depth))]) 
                       (if (leaf? anode) 
                           (values (add1 l) (cons anode achildren)) 
                           (values (+ l (attributed-node-num-leaves anode)) (cons anode achildren)))))]) 
        (attributed-node parent 
                         'interior 
                         leaves 
                         depth 
                         achn))))

;Computes the drawable nodes, and horizontal and vertical extents of
;the resulting image
;;draw-tree/private : node uint uint (node -> (uint . uint)) uint uint 
;;                    -> (values drawable-node uint uint)
(define (draw-tree/private parent x y depth dim-calc zoom padding mx my)
  (define-values (bw bh) (dim-calc parent))
  (define-values (w h) (values (* bw zoom) (* bh zoom)))
  (cond 
    [(empty? (node-children parent))
     (define nx (+ padding x))
     (define ny (+ padding y))
     (define xe (+ nx w))
     (define ye (+ ny h))
     (values (drawable-node parent 
                            nx 
                            ny 
                            w 
                            h
                            depth
                            '() 
                            xe 
                            ye)
             (max mx xe)
             (max my ye))]
    [else
     (define child-y (+ y h))
     (define parenty (+ y padding))
     (cond 
       [(= 1 (length (node-children parent))) ;Align parent and child vertically
        (define-values (child cmx cmy) (draw-tree/private (first (node-children parent))
                                                          x 
                                                          (+ parenty h) 
                                                          (add1 depth) 
                                                          dim-calc 
                                                          zoom
                                                          padding
                                                          mx
                                                          my))
        (define-values (cx _) (control-point child 'center 'top))
        (define nx (max (- cx (/ w 2)) (+ x padding)))
        (define ny (+ padding y))
        (define xe (+ nx w))
        (define ye (+ ny h))
        (values (drawable-node parent 
                               nx
                               ny 
                               w 
                               h
                               depth
                               (list child) 
                               cmx 
                               cmy)
                (max cmx xe)
                (max cmy ye))]
       [else
        (define-values (cmx cmy children) 
          (for/fold ([xacc x] [yacc y] [chn '()]) ([child (in-list (node-children parent))])
            (define-values (dchild cmx cmy) (draw-tree/private child 
                                                               xacc 
                                                               (+ parenty h) 
                                                               (add1 depth) 
                                                               dim-calc 
                                                               zoom
                                                               padding
                                                               mx
                                                               my))
            (values (max xacc cmx) (max yacc cmy) (cons dchild chn))))
        (define chn (reverse children))
        (define last-ch (last chn))
        (define xmin (drawable-node-x (first chn)))
        (define xmax cmx)
        (define nx (- (+ xmin (/ (- xmax xmin) 2))
                      (/ w 2)))
        (define ny (+ padding y))
        (define xe (max (+ nx w) cmx))
        (define ye (max (+ ny h) cmy))
        (values (drawable-node parent 
                               nx
                               ny
                               w 
                               h
                               depth
                               chn 
                               xe
                               ye)
                (max xe mx)  
                (max ye my))])]))

;;draw-tree : node [symbol] [uint] [uint] [uint] -> tree-layout 
(define (draw-tree root 
                   #:dimensions-calc [dim-calc (λ (nd) (values CREATE-GRAPH-NODE-DIAMETER
                                                               CREATE-GRAPH-NODE-DIAMETER))]
                   #:padding [padding CREATE-GRAPH-PADDING] 
                   #:zoom [zoom-level 1])
  (define scaled-padding (* padding zoom-level))
  (define-values (dnodes x-ext y-ext) 
    (draw-tree/private root
                       0
                       0
                       0
                       dim-calc
                       zoom-level
                       scaled-padding 
                       0
                       0)) 
  (graph-layout (+ x-ext scaled-padding) 
                (+ y-ext scaled-padding) 
                (list dnodes)))
