#lang racket/base
(provide DEF-WINDOW-WIDTH
         DEF-WINDOW-HEIGHT
         RT-THREAD-ID
         RT-THREAD-SYM
         MIN-SEG-WIDTH
         STROKE-WIDTH
         MIN-SEG-INNER-WIDTH
         DEFAULT-TIME-INTERVAL
         TIMELINE-HEADER-OPACITY
         CONNECTION-LINE-HAT-THRESHOLD
         HAT-HEIGHT
         CREATE-GRAPH-NODE-DIAMETER
         CREATE-GRAPH-PADDING
         CREATE-GRAPH-MIN-ZOOM
         CREATE-GRAPH-MAX-ZOOM
         CREATE-GRAPH-DEFAULT-ZOOM
         CREATE-GRAPH-ZOOM-FACTOR
         CREATE-GRAPH-STATS-OPACITY
         TIMELINE-ROW-HEIGHT
         TIMELINE-MIN-TICK-PADDING
         HEADER-PADDING
         DEFAULT-TIMELINE-WIDTH
         HEADER-HEIGHT
         TOOLTIP-MARGIN
         NO-FUTURE-PARENT)

(define DEF-WINDOW-WIDTH 1500)
(define DEF-WINDOW-HEIGHT 1000)
(define RT-THREAD-ID 0)
(define RT-THREAD-SYM 'runtime-thread)
(define MIN-SEG-WIDTH 10)
(define STROKE-WIDTH 2)
(define MIN-SEG-INNER-WIDTH (- MIN-SEG-WIDTH STROKE-WIDTH))
;Default time interval (in MS) between ticks on the timeline
(define DEFAULT-TIME-INTERVAL (/ 1 10))
(define TIMELINE-HEADER-OPACITY 0.6)
(define CONNECTION-LINE-HAT-THRESHOLD 20)
(define HAT-HEIGHT 9)
(define CREATE-GRAPH-NODE-DIAMETER 10)
(define CREATE-GRAPH-PADDING 5)
(define CREATE-GRAPH-MIN-ZOOM 1)
(define CREATE-GRAPH-MAX-ZOOM 5)
(define CREATE-GRAPH-DEFAULT-ZOOM 3)
(define CREATE-GRAPH-ZOOM-FACTOR .4)
(define CREATE-GRAPH-STATS-OPACITY .4)
(define TIMELINE-ROW-HEIGHT 100)
(define TIMELINE-MIN-TICK-PADDING 10)
(define HEADER-PADDING 5)
(define DEFAULT-TIMELINE-WIDTH 1000)
(define HEADER-HEIGHT 30)
(define TOOLTIP-MARGIN 5)
(define NO-FUTURE-PARENT #f)