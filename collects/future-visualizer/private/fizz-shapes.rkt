#lang racket/base
(require (only-in racket/function identity) 
         "fizz-core.rkt"
         "fizz-layout.rkt")
(provide view  
         nodes
         edges-from
         circle
         rectangle
         label
         compound
         line)

(define (view nds 
              #:edges [edge-getter #f] 
              #:scale-to-bounds [scale-to-canvas? #f]
              [layout (tree)] 
              #:margin [margin 10]
              #:back-color [back-color "white"]
              #:fore-color [fore-color "black"]
              #:stroke-width [stroke-width 0]
              #:stroke-color [stroke-color "black"]
              #:opacity [opacity 1.0]
              . interactions)
  (λ (data)
    (define vw (_view data 
                      '() '() '() 
                      back-color 
                      fore-color 
                      stroke-width 
                      stroke-color 
                      opacity
                      margin
                      scale-to-canvas? 
                      layout))
    ;Nodes
    (define root-nodes ;listof _node
      (cond 
        [(list? nds) ((nodes (λ (d) nds)) data)]
        [else (nds data)]))
    (for ([n (in-list root-nodes)])
      (set-_element-parent! n vw))
    (set-_node-children! vw root-nodes)
    (when edge-getter
      (for ([n (in-list root-nodes)])
        (define es (edge-getter n root-nodes))
        (for ([e (in-list es)])
          (set-_element-parent! e vw)
          (set-_node-children! vw (cons e (_node-children vw))))))
    vw))

;Find all nodes n (from nodes) for which (node-data n) is equal 
;to some element of vs.
;;find-nodes : (listof any) (listof node) -> (listof node)
(define (find-nodes vs nodes) 
  (define ns (map (λ (v) (findf (λ (n) (equal? (_node-data n) v)) nodes)) vs))
  (filter identity ns)) 

(define (nodes get-node-values
               #:shape [shape (rectangle #:width 20 #:height 20)]
               #:foreach [foreach #f])
  (λ (data)
    (define rootvs 
      (cond 
        [(procedure? get-node-values) (get-node-values data)]
        [else get-node-values]))
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
                    #:shape [shape (line)])
  (λ (from-node all-nodes)
    (define tos (get-to-values (_node-data from-node)))
    (define to-nodes (find-nodes (if (void? tos) '() tos) all-nodes))
    (for/list ([tn (in-list to-nodes)])
      (define e (shape from-node tn))
      (set-_element-parent! e (_element-parent from-node))
      (set-_node-to-edges! tn (cons e (_node-to-edges tn)))
      (set-_node-from-edges! from-node (cons e (_node-from-edges from-node)))
      e)))

(define (compound #:back-color [back-color "white"]
                  #:fore-color [fore-color "black"]
                  #:stroke-width [stroke-width 0]
                  #:stroke-color [stroke-color "black"]
                  #:opacity [opacity 1] 
                  . shapes)
  (λ (data) 
    (define shps (map (λ (s) (s data)) shapes))
    (_compound data 
               shps ;children
               '() ;from-edges
               '() ;to-edges
               back-color 
               fore-color 
               stroke-width 
               stroke-color
               opacity)))

(define (circle #:diameter [diameter (auto 10)]
                #:back-color [back-color "blue"]
                #:fore-color [fore-color "white"]
                #:stroke-width [stroke-width 0]
                #:stroke-color [stroke-color "black"]
                #:opacity [opacity 1])
  (λ (data)
    (_circle data 
             '() ;children
             '() ;from-edges
             '() ;to-edges
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
                '() ;children
                '() ;from-edges
                '() ;to-edges
                back-color
                fore-color
                stroke-width
                stroke-color
                opacity
                width
                height)))

(define (label txt
               #:back-color [back-color "white"]
               #:fore-color [fore-color "black"]
               #:stroke-width [stroke-width 0]
               #:stroke-color [stroke-color "black"]
               #:opacity [opacity 1])
  (λ (data) 
    (_label data
            '()
            '()
            '()
            back-color
            fore-color
            stroke-width
            stroke-color
            opacity 
            (format "~a" txt))))

(define (line #:color [color "black"]
              #:opacity [opacity 1.0])
  (λ (from to)
    (_line from to color opacity)))