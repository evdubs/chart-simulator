#lang racket/base

(require math/statistics
         racket/gui
         (only-in srfi/19 date->string
                  time-utc->date
                  add-duration
                  subtract-duration
                  date->time-utc
                  make-time
                  time-duration
                  string->date)
         "chart.rkt"
         "db-queries.rkt"
         "strategy/ascending-triangle.rkt"
         "strategy/bear-rally.rkt"
         "strategy/bull-pullback.rkt"
         "strategy/descending-triangle.rkt"
         "strategy/high-base.rkt"
         "strategy/low-base.rkt"
         "strategy/range-pullback.rkt"
         "strategy/range-rally.rkt"
         "structs.rkt")

(provide show-simulator)

(struct trade-with-exit (symbol date price amount exit-date exit-price test)
  #:transparent)

(struct test-with-symbol (symbol date entry stop target)
  #:transparent)

(define (trade-with-exit-history symbol trade-history)
  (if (or (empty? trade-history)
          (= 1 (length trade-history)))
      (list)
      (let ([first (first trade-history)]
            [second (second trade-history)])
        (append (list (trade-with-exit symbol
                                       (trade-date first)
                                       (trade-price first)
                                       (trade-amount first)
                                       (trade-date second)
                                       (trade-price second)
                                       (trade-test first)))
                (trade-with-exit-history symbol (list-tail trade-history 2))))))

(define (trade-with-exit-history->ratios tweh)
  (map (λ (e) (real->decimal-string (/ (- (test-target (trade-with-exit-test e)) (trade-with-exit-price e))
                                       (- (test-entry (trade-with-exit-test e)) (test-stop (trade-with-exit-test e))))))
       tweh))

(define (trade-with-exit-history->risks tweh)
  (map (λ (e) (real->decimal-string (* (- (test-entry (trade-with-exit-test e)) (test-stop (trade-with-exit-test e)))
                                       (trade-with-exit-amount e))))
       tweh))

(define (trade-with-exit-history->rewards tweh)
  (map (λ (e) (real->decimal-string (* (- (test-target (trade-with-exit-test e)) (test-entry (trade-with-exit-test e)))
                                       (trade-with-exit-amount e))))
       tweh))

(define (trade-with-exit-history->pcts tweh)
  (map (λ (e) (* (/ (- (trade-with-exit-exit-price e) (trade-with-exit-price e))
                    (trade-with-exit-price e))
                 (trade-with-exit-amount e) 100))
       tweh))

(define (trade-with-exit-history->pcts-str tweh)
  (map (λ (e) (real->decimal-string e))
       (trade-with-exit-history->pcts tweh)))

(define (display-low-base-execution symbol lbe)
  (displayln symbol)
  (displayln "Test History")

  (map (λ (e) (printf "Date: ~a Timeframe: ~a Entry: ~a Stop: ~a Target: ~a\n"
                      (date->string (seconds->date (dv-date e)) "~1")
                      (test-timeframe (dv-value e))
                      (real->decimal-string (test-entry (dv-value e)))
                      (real->decimal-string (test-stop (dv-value e)))
                      (real->decimal-string (test-target (dv-value e)))))
       (history-test lbe))

  (displayln "Trade History")

  (map (λ (e) (printf "Date: ~a Price: ~a Amount: ~a Timeframe: ~a Entry: ~a Stop: ~a Target: ~a\n"
                      (date->string (seconds->date (trade-date e)) "~1")
                      (real->decimal-string (trade-price e))
                      (trade-amount e)
                      (real->decimal-string (test-timeframe (trade-test e)))
                      (real->decimal-string (test-entry (trade-test e)))
                      (real->decimal-string (test-stop (trade-test e)))
                      (real->decimal-string (test-target (trade-test e)))))
       (history-trade lbe)))

(define simulator-frame (new frame% [label "Simulator"] [width 1000] [height 1000]))

(define simulator-input-pane (new horizontal-pane%
                                  [parent simulator-frame]
                                  [stretchable-height #f]))

(define start-date-field
  (new text-field%
       [parent simulator-input-pane]
       [label "Start Date"]
       [init-value "2000-01-01"]))

(define end-date-field
  (new text-field%
       [parent simulator-input-pane]
       [label "End Date"]
       [init-value "2018-01-01"]))

(define random-above-25-str "From 2000 above $25")

(define random-sp-500-str "From current S&P 500")

(define symbol-source-choice
  (new choice%
       [parent simulator-input-pane]
       [label "Symbol Source"]
       [choices (list random-above-25-str random-sp-500-str)]))

(define strategy-hash
  (hash "Bull Pullback" bull-pullback-execution
        "Bear Rally" bear-rally-execution
        "High Base" high-base-execution
        "Low Base" low-base-execution
        "Ascending Triangle" ascending-triangle-execution
        "Descending Triangle" descending-triangle-execution
        "Range Pullback" range-pullback-execution
        "Range Rally" range-rally-execution))

(define strategy-choice
  (new choice%
       [parent simulator-input-pane]
       [label "Strategy"]
       [choices (sort (hash-keys strategy-hash) string<?)]))

(define simulator-get-1-button
  (new button%
       [parent simulator-input-pane]
       [label "Get 1"]
       [callback (λ (c e)
                   (send c enable #f)
                   (let* ([symbol (first (cond
                                           [(equal? (send symbol-source-choice get-string-selection)
                                                    random-above-25-str)
                                            (get-random-symbols-over-price 25.0 1)]
                                           [(equal? (send symbol-source-choice get-string-selection)
                                                    random-sp-500-str)
                                            (get-random-sp-500-symbols 1)]))]
                          [exec ((hash-ref strategy-hash (send strategy-choice get-string-selection))
                                 (get-date-ohlc symbol (send start-date-field get-value)
                                                (send end-date-field get-value)))]
                          [tweh (trade-with-exit-history symbol (history-trade exec))]
                          [winners (filter (λ (t) (<= 0 (* (- (trade-with-exit-exit-price t)
                                                              (trade-with-exit-price t))
                                                           (trade-with-exit-amount t)))) tweh)]
                          [win-pct (if (= 0 (length tweh)) 0
                                       (* (/ (length winners) (length tweh)) 100))]
                          [win-pct-avg (if (= 0 (length winners)) 0
                                           (mean (trade-with-exit-history->pcts winners)))]
                          [losers (remove* winners tweh)]
                          [lose-pct (if (= 0 (length tweh)) 0
                                        (* (/ (length losers) (length tweh)) 100))]
                          [lose-pct-avg (if (= 0 (length losers)) 0
                                            (mean (trade-with-exit-history->pcts losers)))]
                          [reward-ratio (if (= 0 lose-pct-avg) 0 (/ win-pct-avg (abs lose-pct-avg)))]
                          [return (+ (* win-pct win-pct-avg) (* lose-pct lose-pct-avg))])
                     ; (display-low-base-execution symbol lbe)
                     ; (displayln tweh)
                     (send simulator-trades-box set
                           (map (λ (e) (trade-with-exit-symbol e)) tweh)
                           (map (λ (e) (date->string (seconds->date (trade-with-exit-date e)) "~1")) tweh)
                           (map (λ (e) (real->decimal-string (trade-with-exit-price e))) tweh)
                           (map (λ (e) (real->decimal-string (test-entry (trade-with-exit-test e)))) tweh)
                           (map (λ (e) (real->decimal-string (test-stop (trade-with-exit-test e)))) tweh)
                           (map (λ (e) (real->decimal-string (test-target (trade-with-exit-test e)))) tweh)
                           (map (λ (e) (date->string (seconds->date (trade-with-exit-exit-date e)) "~1")) tweh)
                           (map (λ (e) (real->decimal-string (trade-with-exit-exit-price e))) tweh)
                           (trade-with-exit-history->ratios tweh)
                           (trade-with-exit-history->risks tweh)
                           (trade-with-exit-history->rewards tweh)
                           (trade-with-exit-history->pcts-str tweh))
                     (map (λ (t i) (send simulator-trades-box set-data i
                                         (list symbol (trade-with-exit-date t))))
                          tweh (range (length tweh)))
                     (send simulator-trades-box set-label
                           (string-append "Trades History - Reward Ratio: "
                                          (real->decimal-string reward-ratio)
                                          " Win Pct: " (real->decimal-string win-pct)
                                          " Lose Pct: " (real->decimal-string lose-pct)
                                          " Win Pct Avg: " (real->decimal-string win-pct-avg)
                                          " Lose Pct Avg: " (real->decimal-string lose-pct-avg)
                                          " Return: " (real->decimal-string return)))
                     (send simulator-test-box set
                           (map (λ (e) symbol) (history-test exec))
                           (map (λ (e) (date->string (seconds->date (dv-date e)) "~1")) (history-test exec))
                           (map (λ (e) (real->decimal-string (test-entry (dv-value e)))) (history-test exec))
                           (map (λ (e) (real->decimal-string (test-stop (dv-value e)))) (history-test exec))
                           (map (λ (e) (real->decimal-string (test-target (dv-value e)))) (history-test exec)))
                     (map (λ (t i) (send simulator-test-box set-data i
                                         (list symbol (dv-date t))))
                          (history-test exec) (range (length (history-test exec)))))
                   (send c enable #t))]))

(define simulator-get-40-button
  (new button%
       [parent simulator-input-pane]
       [label "Get 40"]
       [callback (λ (c e)
                   (send c enable #f)
                   (let* ([symbols (cond
                                     [(equal? (send symbol-source-choice get-string-selection)
                                              random-above-25-str)
                                      (get-random-symbols-over-price 25.0 40)]
                                     [(equal? (send symbol-source-choice get-string-selection)
                                              random-sp-500-str)
                                      (get-random-sp-500-symbols 40)])]
                          [execs (map (λ (s) (list s ((hash-ref strategy-hash (send strategy-choice get-string-selection))
                                                      (get-date-ohlc s (send start-date-field get-value)
                                                                     (send end-date-field get-value))))) symbols)]
                          [tweh (flatten (map (λ (lbe) (trade-with-exit-history
                                                        (first lbe)
                                                        (history-trade (second lbe))))
                                              execs))]
                          [tws (flatten (map (λ (lbe) (map (λ (t) (test-with-symbol (first lbe)
                                                                                    (dv-date t)
                                                                                    (test-entry (dv-value t))
                                                                                    (test-stop (dv-value t))
                                                                                    (test-target (dv-value t))))
                                                           (history-test (second lbe))))
                                             execs))]
                          [winners (filter (λ (t) (<= 0 (* (- (trade-with-exit-exit-price t)
                                                              (trade-with-exit-price t))
                                                           (trade-with-exit-amount t)))) tweh)]
                          [win-pct (if (= 0 (length tweh)) 0
                                       (* (/ (length winners) (length tweh)) 100))]
                          [win-pct-avg (if (= 0 (length winners)) 0
                                           (mean (trade-with-exit-history->pcts winners)))]
                          [losers (remove* winners tweh)]
                          [lose-pct (if (= 0 (length tweh)) 0
                                        (* (/ (length losers) (length tweh)) 100))]
                          [lose-pct-avg (if (= 0 (length losers)) 0
                                            (mean (trade-with-exit-history->pcts losers)))]
                          [reward-ratio (if (= 0 lose-pct-avg) 0 (/ win-pct-avg (abs lose-pct-avg)))]
                          [return (+ (* win-pct win-pct-avg) (* lose-pct lose-pct-avg))])
                     (send simulator-trades-box set
                           (map (λ (e) (trade-with-exit-symbol e)) tweh)
                           (map (λ (e) (date->string (seconds->date (trade-with-exit-date e)) "~1")) tweh)
                           (map (λ (e) (real->decimal-string (trade-with-exit-price e))) tweh)
                           (map (λ (e) (real->decimal-string (test-entry (trade-with-exit-test e)))) tweh)
                           (map (λ (e) (real->decimal-string (test-stop (trade-with-exit-test e)))) tweh)
                           (map (λ (e) (real->decimal-string (test-target (trade-with-exit-test e)))) tweh)
                           (map (λ (e) (date->string (seconds->date (trade-with-exit-exit-date e)) "~1")) tweh)
                           (map (λ (e) (real->decimal-string (trade-with-exit-exit-price e))) tweh)
                           (trade-with-exit-history->ratios tweh)
                           (trade-with-exit-history->risks tweh)
                           (trade-with-exit-history->rewards tweh)
                           (trade-with-exit-history->pcts-str tweh))
                     (map (λ (t i) (send simulator-trades-box set-data i
                                         (list (trade-with-exit-symbol t) (trade-with-exit-date t))))
                          tweh (range (length tweh)))
                     (send simulator-trades-box set-label
                           (string-append "Trades History - Reward Ratio: " (real->decimal-string reward-ratio)
                                          " Win Pct: " (real->decimal-string win-pct)
                                          " Lose Pct: " (real->decimal-string lose-pct)
                                          " Win Pct Avg: " (real->decimal-string win-pct-avg)
                                          " Lose Pct Avg: " (real->decimal-string lose-pct-avg)
                                          " Return: " (real->decimal-string return)))
                     (send simulator-test-box set
                           (map (λ (t) (test-with-symbol-symbol t)) tws)
                           (map (λ (t) (date->string (seconds->date (test-with-symbol-date t)) "~1")) tws)
                           (map (λ (t) (real->decimal-string (test-with-symbol-entry t))) tws)
                           (map (λ (t) (real->decimal-string (test-with-symbol-stop t))) tws)
                           (map (λ (t) (real->decimal-string (test-with-symbol-target t))) tws))
                     (map (λ (t i) (send simulator-test-box set-data i
                                         (list (test-with-symbol-symbol t) (test-with-symbol-date t))))
                          tws (range (length tws))))
                   (send c enable #t))]))

(define simulator-table-pane (new vertical-pane%
                                  [parent simulator-frame]))

(define (add-months d n)
  (date->string (time-utc->date (add-duration (date->time-utc (seconds->date d))
                                              (make-time time-duration 0 (* 60 60 24 30 n)))) "~1"))

(define (subtract-months d n)
  (date->string (time-utc->date (subtract-duration (date->time-utc (seconds->date d))
                                                   (make-time time-duration 0 (* 60 60 24 30 n)))) "~1"))

(define simulator-trades-box-columns (list "Symbol" "Date" "Price" "Entry Price" "Stop Price" "Target Price" "Exit Date"
                                           "Exit Price" "Ratio" "Risk" "Reward" "Pct Gain"))

(define simulator-trades-box (new list-box%
                                  [label "Trade History"]
                                  [parent simulator-table-pane]
                                  [callback (λ (b e)
                                              (let ([symbol (first (send b get-data (first (send b get-selections))))]
                                                    [date (second (send b get-data (first (send b get-selections))))])
                                                (refresh-chart symbol
                                                               (subtract-months date 5)
                                                               (add-months date 3))))]
                                  [style (list 'single 'column-headers 'vertical-label)]
                                  [columns simulator-trades-box-columns]
                                  [choices (list "")]))

(define simulator-test-box-columns (list "Symbol" "Date" "Entry Price" "Stop Price" "Target Price"))

(define simulator-test-box (new list-box%
                                [label "TEST History"]
                                [parent simulator-table-pane]
                                [callback (λ (b e) 
                                            (let ([symbol (first (send b get-data (first (send b get-selections))))]
                                                  [date (second (send b get-data (first (send b get-selections))))])
                                              (refresh-chart symbol
                                                             (subtract-months date 5)
                                                             (add-months date 3))))]
                                [style (list 'single 'column-headers 'vertical-label)]
                                [columns simulator-test-box-columns]
                                [choices (list "")]))

(define (show-simulator)
  (send simulator-frame show #t)
  (let ([box-width (send simulator-trades-box get-width)]
        [num-cols (length simulator-trades-box-columns)])
    (for-each (λ (i) (send simulator-trades-box set-column-width i
                           100
                           100
                           100)) (range num-cols)))
  (let ([box-width (send simulator-test-box get-width)]
        [num-cols (length simulator-test-box-columns)])
    (for-each (λ (i) (send simulator-test-box set-column-width i
                           250
                           250
                           250)) (range num-cols))))
