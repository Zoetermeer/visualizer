#lang racket/base

(require racket/list
         racket/contract
         (only-in racket/match match-define)

         "constants.rkt")

(provide (struct-out point)
         (struct-out node)
         (struct-out drawable-node)
         (struct-out graph-layout)
         (struct-out attributed-node)
         draw-tree
         drawable-node-center
         build-attr-tree)

(define-struct/contract point ([x integer?] [y integer?]) #:transparent)
(struct node (data children))
(struct graph-layout (width height nodes) #:transparent)
(struct drawable-node (node x y width height depth children children-xextent children-yextent) #:transparent)

(define (int x) 
  (floor (exact->inexact x)))

;;Gets the center point of a node circle.
;;drawable-node-center : node -> point
(define (drawable-node-center dnode) 
  (point (int (+ (drawable-node-x dnode) (/ (drawable-node-width dnode) 2))) 
         (int (+ (drawable-node-y dnode) (/ (drawable-node-width dnode) 2)))))

;;draw-tree/private : node uint uint (node -> (uint . uint)) uint -> drawable-node
(define (draw-tree/private parent x y depth dim-calc padding)
  (match-define (cons w h) (dim-calc parent))
  (cond 
    [(empty? (node-children parent))
     (drawable-node parent 
                    (+ padding x) 
                    (+ padding y) 
                    w 
                    depth
                    '() 
                    (+ padding x w) 
                    (+ padding y h))]
    [else
     (define child-y (+ y h))
     (define children (node-children parent))
     (define parenty (+ y padding))
     (cond 
       [(= 1 (length children)) ;Align parent and child vertically
        (define child (draw-tree/private (first children)
                                         x 
                                         (+ parenty h) 
                                         (add1 depth) 
                                         dim-calc 
                                         padding)) 
        (drawable-node parent 
                       (drawable-node-x child) 
                       parenty 
                       w 
                       h
                       depth
                       (list child) 
                       (drawable-node-children-xextent child) 
                       (drawable-node-children-yextent child))]
       [else
        (define-values (x-extent y-extent children) 
          (for/fold ([xacc x] [yacc y] [chn '()]) ([child (in-list children)])
            (define dchild (draw-tree/private child 
                                              xacc 
                                              (+ parenty h) 
                                              (add1 depth) 
                                              dim-calc 
                                              padding))
              (values (drawable-node-children-xextent dchild) 
                      (drawable-node-children-yextent dchild)
                      (cons dchild chn))))
        (define chn (reverse children))
        (define xmin (drawable-node-x (first chn)))
        (define xmax (drawable-node-x (last chn)))
        (drawable-node parent 
                       (+ xmin (/ (- xmax xmin) 2))
                       parenty
                       w 
                       h
                       depth
                       chn 
                       x-extent 
                       (+ y-extent h))])]))

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



;;draw-tree : node [symbol] [uint] [uint] [uint] -> tree-layout 
(define (draw-tree root 
                   #:node-width [node-width CREATE-GRAPH-NODE-DIAMETER] 
                   #:padding [padding CREATE-GRAPH-PADDING] 
                   #:zoom [zoom-level 1])
  (let* ([scaled-node-w (* node-width zoom-level)] 
         [scaled-padding (* padding zoom-level)] 
         [layout (calc-tree-layout (draw-tree/private root 
                                                       0 
                                                       0 
                                                       0
                                                       scaled-node-w 
                                                       scaled-padding) 
                                   scaled-node-w 
                                   scaled-padding)]) 
    (graph-layout (+ (graph-layout-width layout) scaled-padding) 
                  (+ (graph-layout-height layout) scaled-padding) 
                  (graph-layout-nodes layout))))
