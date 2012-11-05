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

(define-syntax (define-visualization stx)
  (syntax-parse stx
                [(dv name:id 
                     (~optional (param:id ...))
                     (~optional (~seq #:nodes nodes:expr)) 
                     (~optional (~seq #:edges edges:expr))
                     (~optional (~seq #:view view:expr))
                     #:layout layout:expr)
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
                                 [get-edges-stx (if (attribute edges)
                                                    #'(valid-getter edges)
                                                    #'(valid-getter '()))]
                                 [get-view-stx (if (attribute view)
                                                   #'(valid-getter view)
                                                   #'(valid-getter #f))])
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
                                              (define nds (map (λ (v) (node v '() #f)) (get-nodes-stx data)))
                                              (for ([n (in-list nds)])
                                                (define nd (node-data n))
                                                (parameterize ([current-node-data nd])
                                                  (set-node-connections! n (find-nodes (get-edges-stx nd) nds))
                                                  (set-node-view! n (get-view-stx nd))))
                                              (visualization `name 
                                                             nds
                                                             ((layout-drawer layout) data nds))))]))))]))

;Future stuff
(define-syntax (barricades stx) 
  (syntax-case stx ()
    [(barricades ft) #'(future-barricades ft)]
    [_ #'(future-barricades (current-node-data))]))

(define-syntax (allocs stx)
  (syntax-case stx ()
    [(allocs ft) #'(future-allocs ft)]
    [_ #'(future-allocs (current-node-data))]))

(define-syntax (spawned-futures stx)
  (syntax-case stx ()
    [(spawned-futures ft) #'(future-spawned-futures ft)]
    [_ #'(future-spawned-futures (current-node-data))]))

(define-syntax (real-time stx)
  (syntax-case stx ()
    [(real-time ft) #'(future-real-time ft)]
    [_ #'(future-real-time (current-node-data))]))

;Thread stuff
(define-syntax (all-events stx)
  (syntax-case stx ()
    [(all-events thd) #'(thread-all-events thd)]
    [_ #'(thread-all-events (current-visualization-data))]))

;Event stuff
(define-syntax (previous-event stx)
  (syntax-case stx ()
    [(previous-event evt) #'(event-previous-event thd)]
    [_ #'(event-previous-event (current-node-data))]))

;Execution stuff
(define-syntax (all-futures stx)
  (syntax-case stx ()
    [(all-futures vd) #'(execution-all-futures vd)]
    [_ #'(execution-all-futures (current-visualization-data))]))

(define-syntax (execution-start-time stx)
  (syntax-case stx ()
    [(execution-start-time exec) #'(exec-start-time exec)]
    [_ #'(exec-start-time (current-visualization-data))]))

(define-syntax (execution-end-time stx)
  (syntax-case stx ()
    [(execution-start-time exec) #'(exec-end-time exec)]
    [_ #'(exec-end-time (current-visualization-data))]))

(define (tree-layout data nodes)
  42)

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

(define-visualization circle (diameter color)
  #:layout (λ (vis-data nodes)
             (colorize (filled-ellipse diameter diameter) color)))

(define-visualization rect (width height color text)
  #:layout (λ (vis-data nodes)
             (cond 
               [text 
                (define t (pict-text text))
                (define r (colorize (filled-rectangle (if (auto? width)
                                                          (pict-width t)
                                                          width)
                                                      (if (auto? height)
                                                          (pict-height t)
                                                          height))
                                    color))
                (cc-superimpose r t)]
               [else
                (cond 
                  [(or (auto? width) (auto? height))
                   (error 'rect "Width/height cannot be auto if container is empty.")]
                  [else
                   (colorize (filled-rectangle width height) color)])])))
             
;The left-hand hierlist view with barricade/sync/gc items
#;(define-visualization summary-view
  #:nodes (append (list "Blocks" "Syncs" "GC's")
                  all-unsafe-primitives)
  #:edges (λ (node)
            (case (node-data node)
              ["Blocks" all-barricade-primitives]
              ["Syncs" all-sync-primitives]
              ["GC's" all-gcs]
              [else '()]))
  #:view to-string
  #:layout hierarchical-list)

#;(define-visualization creation-graph
  #:nodes all-futures
  #:edges spawned-futures
  #:view (rect #:width (length barricades)
               #:height real-time
               #:color (if (zero? (length allocs)) "blue" "red"))
  #:layout tree-layout)

#;(define-visualization thread-timeline
  #:nodes all-events
  #:edges (list previous-event next-event)
  #:view (circle #:width 20
                 #:height 20
                 #:color "green")
  #:layout elastic-timeline)

#;(define-visualization execution-timeline
  #:nodes all-threads
  #:view thread-timeline
  #:layout (stack #:orientation 'vertical))

#;(define-visualization util-history 
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
    #:view (λ (node) 
             (define d (node-data node))
             (rect #:width 'auto
                 #:height 'auto
                 #:color "blue" 
                 #:text (format "~a - ~a" (car d) (cdr d))))
    #:layout (stack #:orientation 'horizontal))
                  
                
              
              


(define ex (exec 0.1
                 25.0
                 (list (future 1 10.0 '(2) '() '() '()) 
                       (future 2 20.0 '() '() '() '())
                       (future 3 10.0 '(4) '() '() '())
                       (future 4 25.0 '() '() '() '()))))
;((creation-graph) ex)
;((rect #:width 300 #:height 400 #:color "red") #f)                            





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