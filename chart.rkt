#lang racket

(require plot
         (only-in srfi/19
                  make-time
                  string->date
                  time-utc->date
                  add-duration
                  time-duration
                  date->time-utc
                  date->string)
         racket/gui
         "db-queries.rkt"
         "plot-util.rkt"
         "structs.rkt"
         "technical-indicators.rkt")

(provide show-chart
         refresh-chart)

(define (refresh-chart symbol start-date end-date)
  (send chart-ticker-symbol-field set-value symbol)
  (send chart-start-date-field set-value start-date)
  (send chart-end-date-field set-value end-date)
  (send chart-price-canvas set-snip (chart-price-plot))
  (send chart-atr-canvas set-snip (chart-atr-plot))
  (send chart-volume-canvas set-snip (chart-volume-plot)))

(define (next-day d)
  (date->string (time-utc->date (add-duration (date->time-utc (string->date d "~Y-~m-~d"))
                                              (make-time time-duration 0 (* 60 60 24)))) "~1"))

(plot-y-tick-labels? #f)
(plot-y-far-tick-labels? #t)

(define chart-frame (new frame% [label "Chart"] [width 1400] [height 1000]))

(define chart-input-pane (new horizontal-pane%
                            [parent chart-frame]
                            [stretchable-height #f]))

(define chart-ticker-symbol-field (new text-field%
                                     [parent chart-input-pane]
                                     [label "Symbol"]
                                     [init-value "GE"]))

(define chart-start-date-field (new text-field%
                                  [parent chart-input-pane]
                                  [label "Start Date"]
                                  [init-value "2018-01-01"]))

(define chart-end-date-field (new text-field%
                                [parent chart-input-pane]
                                [label "End Date"]
                                [init-value "2018-06-30"]))

(define chart-refresh-button (new button%
                                [parent chart-input-pane]
                                [label "Refresh"]
                                [callback (λ (b e) (send chart-price-canvas set-snip (chart-price-plot))
                                            (send chart-atr-canvas set-snip (chart-atr-plot))
                                            (send chart-volume-canvas set-snip (chart-volume-plot)))]))

(define next-day-button (new button%
                             [parent chart-input-pane]
                             [label "Next Day"]
                             [callback (λ (b e) (send chart-start-date-field set-value (next-day (send chart-start-date-field get-value)))
                                         (send chart-end-date-field set-value (next-day (send chart-end-date-field get-value)))
                                         (send chart-price-canvas set-snip (chart-price-plot))
                                         (send chart-atr-canvas set-snip (chart-atr-plot))
                                         (send chart-volume-canvas set-snip (chart-volume-plot)))]))

(define chart-plot-pane (new vertical-pane%
                           [parent chart-frame]))

(define (chart-price-plot)
  (let ([date-ohlc-vector (get-date-ohlc (send chart-ticker-symbol-field get-value)
                                         (send chart-start-date-field get-value)
                                         (send chart-end-date-field get-value))])
    (parameterize ([plot-x-ticks (date-ticks)]
                   [plot-y-ticks (currency-ticks #:kind 'USD)]
                   [plot-width (- (send chart-price-canvas get-width) 12)]
                   [plot-height (- (send chart-price-canvas get-height) 12)])
      (plot-snip (list (tick-grid)
                       (let-values ([(highs lows) (donchian-channel (list->vector date-ohlc-vector) 50)])
                         (lines-interval highs lows #:color 7 #:alpha 1/3 #:label "50-day DC"))
                       (let-values ([(highs lows) (donchian-channel (list->vector date-ohlc-vector) 10)])
                         (lines-interval highs lows #:color 6 #:alpha 1/3 #:label "10-day DC"))
                       (candlesticks date-ohlc-vector #:width 86400)
                       (lines (simple-moving-average (list->vector date-ohlc-vector) 20) #:color 3 #:label "20-day SMA")
                       (lines (simple-moving-average (list->vector date-ohlc-vector) 50) #:color 4 #:label "50-day SMA"))
                 #:x-label "Date"
                 #:y-label "Price"))))

(define chart-price-canvas (new settable-snip-canvas% 
                              [parent chart-plot-pane]))

(define (chart-atr-plot)
  (let ([date-ohlc-vector (get-date-ohlc (send chart-ticker-symbol-field get-value)
                                         (send chart-start-date-field get-value)
                                         (send chart-end-date-field get-value))])
    (parameterize ([plot-x-label #f]
                   [plot-x-ticks (date-ticks)]
                   [plot-y-ticks (currency-ticks #:kind 'USD)]
                   [plot-width (- (send chart-atr-canvas get-width) 12)]
                   [plot-height (- (send chart-atr-canvas get-height) 12)])
      (plot-snip (list (tick-grid)
                       (lines (simple-average-true-range (list->vector date-ohlc-vector) 50)
                              #:x-min (dohlc-date (first date-ohlc-vector))))
                 #:y-label "50-day SATR"))))

(define chart-atr-canvas (new settable-snip-canvas% 
                            [parent chart-plot-pane]
                            [min-height 150]
                            [stretchable-height #f]))

(define (chart-volume-plot) (parameterize ([plot-x-label #f]
                                         [plot-x-ticks (date-ticks)]
                                         [plot-y-ticks (linear-ticks)]
                                         [plot-width (- (send chart-volume-canvas get-width) 12)]
                                         [plot-height (- (send chart-volume-canvas get-height) 12)])
                            (plot-snip (list (tick-grid)
                                             (rectangles (get-date-volume (send chart-ticker-symbol-field get-value)
                                                                          (send chart-start-date-field get-value)
                                                                          (send chart-end-date-field get-value))
                                                         #:color 3))
                                       #:y-label "Volume")))

(define chart-volume-canvas (new settable-snip-canvas% 
                               [parent chart-plot-pane]
                               [min-height 150]
                               [stretchable-height #f]))

(define (show-chart)
  (send chart-frame show #t))
