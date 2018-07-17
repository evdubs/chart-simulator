#lang racket

(provide dv
         dv-date
         dv-value
         dohlc
         dohlc-date
         dohlc-open
         dohlc-high
         dohlc-low
         dohlc-close
         test
         test-timeframe
         test-entry
         test-stop
         test-target
         test-timeframe-minus-1
         trade
         trade-date
         trade-price
         trade-amount
         trade-test
         position
         position-price
         position-amount
         history
         history-test
         history-trade
         high-base-in
         high-base-in-dohlc
         high-base-in-sma-20
         high-base-in-sma-20-slope
         high-base-in-sma-50
         high-base-in-sma-50-slope
         high-base-in-satr-50
         high-base-in-dc-10-high
         high-base-in-dc-10-low
         high-base-in-dc-50-high
         high-base-in-dc-50-low
         high-base-in-drop-1
         low-base-in
         low-base-in-dohlc
         low-base-in-sma-20
         low-base-in-sma-20-slope
         low-base-in-sma-50
         low-base-in-sma-50-slope
         low-base-in-satr-50
         low-base-in-dc-10-high
         low-base-in-dc-10-low
         low-base-in-dc-50-high
         low-base-in-dc-50-low
         low-base-in-drop-1)

(struct dv (date value)
  #:transparent
  #:methods gen:stream
  [(define (stream-empty? stream)
     (cond
       [(dv? stream) #f]
       [else (empty? stream)]))
   (define (stream-first stream)
     (cond
       [(dv? stream) (dv-date stream)]
       [else (first stream)]))
   (define (stream-rest stream)
     (cond
       [(dv? stream) (list (dv-value stream))]
       [else (rest stream)]))])

(struct dohlc (date open high low close)
  #:transparent
  #:methods gen:stream
  [(define (stream-empty? stream)
     (cond
       [(dohlc? stream) #f]
       [else (empty? stream)]))
   (define (stream-first stream)
     (cond
       [(dohlc? stream) (dohlc-date stream)]
       [else (first stream)]))
   (define (stream-rest stream)
     (cond
       [(dohlc? stream) (list (dohlc-open stream)
                              (dohlc-high stream)
                              (dohlc-low stream)
                              (dohlc-close stream))]
       [else (rest stream)]))])

(struct test (timeframe entry stop target)
  #:transparent)

(define (test-timeframe-minus-1 t)
  (test (- (test-timeframe t) 1)
        (test-entry t)
        (test-stop t)
        (test-target t)))

(struct trade (date price amount test)
  #:transparent)

(struct position (price amount)
  #:transparent)

(struct history (test trade)
  #:transparent)

(struct high-base-in (dohlc
                      sma-20
                      sma-20-slope
                      sma-50
                      sma-50-slope
                      satr-50
                      dc-10-high
                      dc-10-low
                      dc-50-high
                      dc-50-low)
  #:transparent)

(define (high-base-in-drop-1 lbi)
  (high-base-in (stream-rest (high-base-in-dohlc lbi))
                (stream-rest (high-base-in-sma-20 lbi))
                (stream-rest (high-base-in-sma-20-slope lbi))
                (stream-rest (high-base-in-sma-50 lbi))
                (stream-rest (high-base-in-sma-50-slope lbi))
                (stream-rest (high-base-in-satr-50 lbi))
                (stream-rest (high-base-in-dc-10-high lbi))
                (stream-rest (high-base-in-dc-10-low lbi))
                (stream-rest (high-base-in-dc-50-high lbi))
                (stream-rest (high-base-in-dc-50-low lbi))))

(struct low-base-in (dohlc
                     sma-20
                     sma-20-slope
                     sma-50
                     sma-50-slope
                     satr-50
                     dc-10-high
                     dc-10-low
                     dc-50-high
                     dc-50-low)
  #:transparent)

(define (low-base-in-drop-1 lbi)
  (low-base-in (stream-rest (low-base-in-dohlc lbi))
               (stream-rest (low-base-in-sma-20 lbi))
               (stream-rest (low-base-in-sma-20-slope lbi))
               (stream-rest (low-base-in-sma-50 lbi))
               (stream-rest (low-base-in-sma-50-slope lbi))
               (stream-rest (low-base-in-satr-50 lbi))
               (stream-rest (low-base-in-dc-10-high lbi))
               (stream-rest (low-base-in-dc-10-low lbi))
               (stream-rest (low-base-in-dc-50-high lbi))
               (stream-rest (low-base-in-dc-50-low lbi))))
