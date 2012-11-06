#lang racket
(require (rename-in slideshow/pict 
                    [circle pict-circle]
                    [rectangle pict-rectangle]
                    [text pict-text])
         "vis-runtime.rkt"
         (for-syntax syntax/parse 
                     "vis-runtime.rkt"
                     (only-in racket/list remove-duplicates)))

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

(define (tree data nodes)
  42)

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

;(define-visualization summary-view (+ 1 2))

;A "top-level" visualization can be viewed as a function 
;with an implicit argument that is some data structure 
;containing information about a future execution. 
;So `all-futures is some function we can apply with that object 
;to get a list of all futures, for example.

;The basic "built-in" views are themselves visualizations
;defined using the same language.
(define (auto? v)
  (case v
    [(auto) #t]
    [else #f]))

;Circle view
(define-view circle (diameter color)
  #:layout-view (λ (vis-data nodes)
                  (colorize (filled-ellipse diameter diameter) color)))

;Rectangle view
;(define-visualization rect (width height color [text #f])  ??
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
             
;The left-hand hierlist view with barricade/sync/gc items
#;(define-view summary-view
  #:nodes (append (list "Blocks" "Syncs" "GC's")
                  all-unsafe-primitives)
  #:out-edges (λ (node)
            (case (node-data node)
              ["Blocks" all-barricade-primitives]
              ["Syncs" all-sync-primitives]
              ["GC's" all-gcs]
              [else '()]))
  #:node-view to-string
  #:layout-view hierarchical-list)

(define-view creation-graph
  #:nodes exec-all-futures
  #:out-edges future-spawned-futures
  #:node-view (λ (ft) 
                ((rect #:width (max 10 (length (future-barricades ft)))
                       #:height (* (future-real-time ft) 10)
                       #:back-color (if (zero? (length (future-allocs ft))) "blue" "red")
                       #:fore-color "white"
                       #:text (future-id ft)) ft))
  #:edge-view (λ (in out) #f)
  #:layout-view #;tree (stack #:orientation 'vertical #:margin 10))

#;(define-visualization thread-timeline
  #:nodes all-events
  #:out-edges (list previous-event next-event)
  #:node-shape (circle #:diameter 20
                 #:color "green"
                 #:text #f)
  #:layout elastic-timeline)

#;(define-view execution-timeline
  #:nodes all-threads
  #:node-view thread-timeline
  #:layout-view (stack #:orientation 'vertical))

#;(define-view util-history 
    #:nodes (λ (exec) 
              (define SAMPLES 100)
              (define interval (/ (- execution-end-time execution-start-time)
                                  SAMPLES))
              (define-values (_ samples) 
                (for/fold ([cur-time execution-start-time]
                           [samples '()]) ([i (in-range SAMPLES)])
                  (values (+ cur-time interval)
                          (cons (cons cur-time (+ cur-time interval))
                                samples))))
              samples)
    #:node-view (λ (node) 
             (define d (node-data node))
             (rect #:width 'auto
                 #:height 'auto
                 #:color "blue" 
                 #:text (format "~a - ~a" (car d) (cdr d))))
    #:layout-view (stack #:orientation 'horizontal))
                  
                
              
              


(define ex (exec 0.1
                 25.0
                 (list (future 1 10.0 '(2) '() '() '()) 
                       (future 2 20.0 '() '() '() '())
                       (future 3 10.0 '(4) '() '() '())
                       (future 4 25.0 '() '() '() '()))))
((creation-graph) ex)
;((rect #:width 300 #:height 400 #:color "red" #:text "foo") #f)                            





#|
(define-visualization FutureBlueprint
  (nodes (all-futures))   ; *all-Futures* is a collection of future objects
  (edges (spawned-futures))   ; (spawn-futures f) -> a collection of created futures
  (shape (rectangle (width numberOfBlockingOperations) ; (numberOfBlockingOperations f) -> integer
                    (height timeSpent)                  ; (timeSpent f) -> integer
                    (color-if hasAllocatedMemory? #red) ; (hasAllocatedMemory? f) -> boolean
                    (color-if hasNotAllocatedMemory? #blue))) 
  (interaction :on #MouseEnter :do (lambda (future) ... ))
  (layout treeLayout))

(define-visualization TimeLine
  (nodes (all-threads) #:foreach ThreadLine)
  (layout vertical-line-layout) )

(define-visualization ThreadLine
  (lambda (thread)
    (nodes (getStartBlocks thread))
    (shape (circle (size 10)
                    (color-if ....)))
    (layout (scatterplotLayout (x timeBegin))))) 

|#