#lang racket
(require (rename-in slideshow/pict 
                    [circle pict-circle]
                    [rectangle pict-rectangle]
                    [text pict-text])
         "fizz-core.rkt"
         (for-syntax syntax/parse 
                     "fizz-core.rkt"
                     (only-in racket/list remove-duplicates)))
(provide define-view
         auto?)

;Find all nodes n (from nodes) for which (node-data n) is equal 
;to some element of vs.
;;find-nodes : (listof any) (listof node) -> (listof node)
(define (find-nodes vs nodes)
  (map (λ (v) (findf (λ (n) (equal? (node-data n) v)) nodes)) vs))

;;valid-getter : any -> (any -> any)
(define (valid-getter p)
  (if (and (procedure? p) (= (procedure-arity p) 1))
      p
      (λ (v) p)))

(define (edge-drawer p)
  (if (and (procedure? p) (= (procedure-arity p) 2))
      p 
      (λ (node-a node-b) p)))

(define (layout-drawer p)
  (if (and (procedure? p) (= (procedure-arity p) 2))
      p 
      (λ (vis-data nodes) p)))

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

(define-syntax (define-view stx)
  (syntax-parse stx
                [(dv name:id 
                     (~optional (param:id ...))
                     (~optional (~seq #:nodes nodes:expr)) 
                     (~optional (~seq #:out-edges out-edges:expr))
                     (~optional (~seq #:node-view node-view:expr))
                     (~optional (~seq #:edge-view edge-view:expr))
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
                                                   #'(valid-getter node-view)
                                                   #'(valid-getter #f))]
                                 [get-edge-view-stx (if (attribute edge-view)
                                                        #'(edge-drawer edge-view)
                                                        #'(edge-drawer #f))])
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
                                       [(_ kws ...)                         
                                        #'(λ (data)
                                            (parameterize ([current-visualization-data data])
                                              (define nds (map (λ (v) (node v '() '())) (get-nodes-stx data)))
                                              (for ([n (in-list nds)])
                                                (define nd (node-data n))
                                                (parameterize ([current-node-data nd])
                                                  (set-node-out-edges! n (find-nodes (get-out-edges-stx nd) nds))
                                                  (set-node-view-drawer! n (get-node-view-stx nd)))) 
                                              ((layout-drawer layout) data nds)
                                              #;(view `name 
                                                    nds
                                                    ((layout-drawer layout) data nds))))]))))]))

;;auto? : any -> boolean
(define (auto? v)
  (case v
    [(auto) #t]
    [else #f]))