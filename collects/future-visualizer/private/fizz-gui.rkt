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
                         (set-view-bounds! vw (rect 0 0 (viewable-region-width vregion) (viewable-region-height vregion)))
                         ((view-layout-drawer vw) vregion))]
         [hover-handler (λ (x y vregion) 
                          (define hovered (let loop ([nds (view-nodes vw)])
                                            (cond 
                                              [(empty? nds) #f]
                                              [else
                                               (define n (car nds))
                                               (define nview (node-view n))
                                               (if (and (between x (view-origin-x nview) (view-x-extent nview))
                                                        (between y (view-origin-y nview) (view-y-extent nview)))
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
                                          ((view-layout-drawer ((interaction-handler inter) (node-view hovered) (node-data hovered)))
                                                               vregion))]
                               [else #f])]))]
         [click-handler (λ (x y vregion) #f)]
         [overlay-builder (λ (vregion scale-factor) #f)]
         [style '(hscroll vscroll)]
         [stretchable-width #t]
         [stretchable-height #t]))
  
  (send f show #t))