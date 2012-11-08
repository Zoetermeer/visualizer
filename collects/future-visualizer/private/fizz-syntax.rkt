#lang racket
(require (rename-in slideshow/pict 
                    [circle pict-circle]
                    [rectangle pict-rectangle]
                    [text pict-text])
         "fizz-core.rkt"
         (for-syntax syntax/parse 
                     "fizz-core.rkt"
                     (only-in racket/list remove-duplicates))
         "display.rkt")
(provide (contract-out 
          [build-view (->*
                       (symbol? 
                        #:layout-view (any/c (listof node?) (or/c viewable-region? #f) . -> . pict?))
                       (#:nodes (any/c . -> . (listof any/c))
                        #:out-edges (any/c . -> . (listof any/c))
                        #:node-view (any/c (or/c viewable-region? #f) . -> . pict?)
                        #:edge-view (node? node? (or/c viewable-region? #f) . -> . pict?)
                        #:scale-to-canvas? boolean?)
                       #:rest (listof interaction?)
                       (any/c (or/c viewable-region? #f) . -> . pict?))]))

;Find all nodes n (from nodes) for which (node-data n) is equal 
;to some element of vs.
;;find-nodes : (listof any) (listof node) -> (listof node)
(define (find-nodes vs nodes)
  (define ns (map (λ (v) (findf (λ (n) (equal? (node-data n) v)) nodes)) vs))
  (when (list? (member #f ns))
    (error 'find-nodes "Nodes could not be found for values in the list: ~a" vs))
  ns)

;;valid-getter : any -> (any -> any)
(define (valid-getter p)
  (if (and (procedure? p) (= (procedure-arity p) 1))
      p
      (λ (v) p)))

(define (node-drawer p)
  (if (and (procedure? p) (= (procedure-arity p) 2))
      (begin (printf "p was already a node-drawer\n") p) 
      (begin (printf "p was not a node-drawer\n") (λ (data vregion) p))))

(define (edge-drawer p)
  (if (and (procedure? p) (= (procedure-arity p) 3))
      p 
      (λ (node-a node-b vregion) p)))

(define (layout-drawer p)
  (if (and (procedure? p) (= (procedure-arity p) 3))
      p 
      (λ (vis-data nodes vregion) p)))

;;duplicate-keywords? : (listof syntax-object) -> boolean
(define-for-syntax (duplicate-keywords? kws)
  (define names (map (λ (stx) (keyword->string (syntax->datum stx))) kws))
  (not (= (length (remove-duplicates names)) (length names))))

;;stx-keyword->id : syntax syntax:keyword -> syntax:id
(define-for-syntax (stx-keyword->id ctx stx-kw)
  (datum->syntax ctx 
                 (string->symbol 
                  (keyword->string 
                   (syntax->datum stx-kw)))))

(define (build-view name 
                    #:nodes [nodes (λ (data) '())]
                    #:out-edges [out-edges (λ (node-data) '())]
                    #:node-view [node-view (λ (data vregion) #f)]
                    #:edge-view [edge-view (λ (tail head vregion) #f)]
                    #:scale-to-canvas? [scale-to-canvas? #f]
                    #:layout-view layout-view 
                    . interactions)
  (λ (data vregion)
    (define nds (map (λ (v) (node v '() '())) (nodes data)))
    (for ([n (in-list nds)])
      (define nd (node-data n))
      (set-node-view-drawer! n (λ () (node-view nd vregion)))
      (define outs (out-edges nd))
      (define out-nodes (find-nodes (out-edges nd) nds))
      (for ([o-n (in-list out-nodes)])
        (define e (edge n o-n (λ () (edge-view n o-n vregion))))
        (set-node-out-edges! n (cons e (node-out-edges n)))
        (set-node-in-edges! o-n (cons e (node-in-edges o-n)))))
    (layout-view data nds vregion)))

#;(define-syntax (define-view stx)
  (syntax-parse stx
                [(dv name:id 
                     (~optional (param:id ...+))
                     (~optional (~seq #:nodes nodes:expr)) 
                     (~optional (~seq #:out-edges out-edges:expr))
                     (~optional (~seq #:node-view node-view:expr))
                     (~optional (~seq #:edge-view edge-view:expr))
                     (~optional (~seq #:sale-to-canvas scale-to-canvas:expr))
                     #:layout-view layout:expr)
                 (with-syntax ([(kws ...) (if (attribute param)
                                              (reverse (for/fold ([params '()]) 
                                                         ([p (in-list (syntax->list #'(param ...)))])
                                                (cons p 
                                                      (cons (string->keyword (symbol->string (syntax->datum p))) 
                                                            params))))
                                              '())])
                   (with-syntax ([fun (if (attribute param)
                                        #'(name kws ...)
                                        #'name)]
                                 [get-nodes-stx (if (attribute nodes)
                                                    #'(valid-getter nodes)
                                                    #'(valid-getter '()))]
                                 [get-out-edges-stx (if (attribute out-edges)
                                                    #'(valid-getter out-edges)
                                                    #'(valid-getter '()))]
                                 [get-node-view-stx (if (attribute node-view)
                                                   #'(node-drawer node-view)
                                                   #'(node-drawer #f))]
                                 [get-edge-view-stx (if (attribute edge-view)
                                                        #'(edge-drawer edge-view)
                                                        #'(edge-drawer #f))]
                                 [get-scale-to-canvas-stx (if (attribute scale-to-canvas)
                                                              #'scale-to-canvas
                                                              #'#f)])
                     #'(define-syntax (name sx)
                         (define-splicing-syntax-class keyword-arg 
                           #:description "keyword argument"
                           (pattern (~seq kw:keyword v:expr)))
                         (define-splicing-syntax-class distinct-kw-args
                           (pattern (~seq kwa:keyword-arg (... ...))
                                    #:fail-when (duplicate-keywords? 
                                                 (syntax->list #'(kwa.kw (... ...))))
                                    "duplicate keyword"
                                    #:with (kw (... ...)) #'(kwa.kw (... ...))
                                    #:with (v (... ...)) #'(kwa.v (... ...))))
                         (syntax-parse sx
                                       [(name:id data:expr vregion:expr)
                                        #'(begin 
                                          (define nds (map (λ (v) (node v '() '())) (get-nodes-stx data)))
                                          (printf "nodes=~a\n" (length nds))
                                          (for ([n (in-list nds)])
                                            (define nd (node-data n))
                                            (set-node-view-drawer! n (λ () (get-node-view-stx nd vregion)))
                                            (define out-nodes (find-nodes (get-out-edges-stx nd) nds))
                                            (for ([o-n (in-list out-nodes)])
                                              (define e (edge n o-n (λ () (get-edge-view-stx n o-n vregion))))
                                              (set-node-out-edges! n (cons e (node-out-edges n)))
                                              (set-node-in-edges! o-n (cons e (node-in-edges o-n)))))
                                          ((layout-drawer layout) data nds vregion))]
                                       [(_ kws ...)                         
                                        #'(λ (data vregion)
                                            (define nds (map (λ (v) (node v '() '())) (get-nodes-stx data)))
                                            (for ([n (in-list nds)])
                                              (define nd (node-data n))
                                              (set-node-view-drawer! n (λ () (get-node-view-stx nd vregion)))
                                              (define out-nodes (find-nodes (get-out-edges-stx nd) nds))
                                              (for ([o-n (in-list out-nodes)])
                                                (define e (edge n o-n (λ () (get-edge-view-stx n o-n vregion))))
                                                (set-node-out-edges! n (cons e (node-out-edges n)))
                                                (set-node-in-edges! o-n (cons e (node-in-edges o-n))))) 
                                            ((layout-drawer layout) data nds vregion))]
                                       [(~var nm:id) 
                                        #'(λ (data vregion)
                                            (define nds (map (λ (v) (node v '() '())) (get-nodes-stx data)))
                                            (for ([n (in-list nds)])
                                              (define nd (node-data n))
                                              (set-node-view-drawer! n (λ () (get-node-view-stx nd vregion)))
                                              (define out-nodes (find-nodes (get-out-edges-stx nd) nds))
                                              (for ([o-n (in-list out-nodes)])
                                                (define e (edge n o-n (λ () (get-edge-view-stx n o-n vregion))))
                                                (set-node-out-edges! n (cons e (node-out-edges n)))
                                                (set-node-in-edges! o-n (cons e (node-in-edges o-n)))))
                                            ((layout-drawer layout) data nds vregion))]))))]))










