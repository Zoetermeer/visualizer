#lang racket/gui
(require framework
         (only-in slideshow/pict pin-over blank)
         "fizz-canvas.rkt"
         "fizz-core.rkt"
         "display.rkt")
(provide visualize)

(define (visualize #:width [width 1000]
                   #:height [height 800] 
                   . views)
  (define f (new frame:standard-menus% 
                 [label "Visualizer"] 
                 [width width] 
                 [height height]))
  (define main-panel (new panel:horizontal-dragable% 
                          [parent (send f get-area-container)] 
                          [stretchable-height #t] 
                          [stretchable-width #t]))
  (for ([vw (in-list views)])
    (new fizz-canvas% 
         [parent main-panel]
         [pict-builder (λ (vregion)
                         (define maybe-lowestp ((car (view-layout-drawers vw)) vregion))
                         (define lowestp (if (not maybe-lowestp) 
                                             (blank (viewable-region-width vregion)
                                                                        (viewable-region-height vregion))
                                             maybe-lowestp))
                         (define p (for/fold ([p lowestp]) ([lyt (in-list (cdr (view-layout-drawers vw)))])
                                     (define layout-p (lyt vregion))
                                     (if (not layout-p)
                                         p
                                         (pin-over p 
                                                   0
                                                   0
                                                   layout-p))))
                         p)]
         [hover-handler (λ (x y vregion) 
                          (define hovered (let loop ([nds (view-nodes vw)])
                                            (cond 
                                              [(empty? nds) #f]
                                              [else
                                               (define n (car nds))
                                               (if (and (between x (node-origin-x n) (node-x-extent n))
                                                        (between y (node-origin-y n) (node-y-extent n)))
                                                   n
                                                   (loop (cdr nds)))])))
                          (cond
                            [(and (not hovered) (not (view-hovered-node vw))) #f]
                            [(eq? hovered (view-hovered-node vw)) #f]
                            [else 
                             (set-view-hovered-node! vw hovered)
                             (define inter (view-interaction-for 'hover vw))
                             (cond 
                               [inter
                                ;Only one level deep for now, but need to handle mouseover 
                                ;in views created by mousing over this node (arbitrary nesting)
                                (pin-over (blank (viewable-region-width vregion)
                                                 (viewable-region-height vregion)) 
                                          0
                                          0
                                          ((interaction-handler inter) hovered vregion))]
                               [else #f])]))]
         [click-handler (λ (x y vregion) #f)]
         [overlay-builder (λ (vregion scale-factor) #f)]
         [style '(hscroll vscroll)]
         [stretchable-width #t]
         [stretchable-height #t]))
  
  (send f show #t))