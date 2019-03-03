#lang racket/base

(require racket/stream
         racket/vector
         "../structs.rkt"
         "../technical-indicators.rkt")

(provide bull-pullback-execution)

(struct bull-pullback-in
  (dohlc
   sma-20
   sma-20-slope
   sma-50
   sma-50-slope
   satr-50
   dc-20-high
   dc-20-low)
  #:transparent)

(define (bull-pullback-in-drop-1 lbi)
  (bull-pullback-in (stream-rest (bull-pullback-in-dohlc lbi))
                    (stream-rest (bull-pullback-in-sma-20 lbi))
                    (stream-rest (bull-pullback-in-sma-20-slope lbi))
                    (stream-rest (bull-pullback-in-sma-50 lbi))
                    (stream-rest (bull-pullback-in-sma-50-slope lbi))
                    (stream-rest (bull-pullback-in-satr-50 lbi))
                    (stream-rest (bull-pullback-in-dc-20-high lbi))
                    (stream-rest (bull-pullback-in-dc-20-low lbi))))

(define (bull-pullback t ; timeframe entry stop target
                       p ; position
                       h ; history
                       i) ; market data inputs
  (if (or (stream-empty? (bull-pullback-in-dohlc i))
          (stream-empty? (bull-pullback-in-sma-20 i))
          (stream-empty? (bull-pullback-in-sma-20-slope i))
          (stream-empty? (bull-pullback-in-sma-50 i))
          (stream-empty? (bull-pullback-in-sma-50-slope i))
          (stream-empty? (bull-pullback-in-satr-50 i))
          (stream-empty? (bull-pullback-in-dc-20-high i))
          (stream-empty? (bull-pullback-in-dc-20-low i)))
      h

      (let ([date (dohlc-date (stream-first (bull-pullback-in-dohlc i)))]
            [open (dohlc-open (stream-first (bull-pullback-in-dohlc i)))]
            [high (dohlc-high (stream-first (bull-pullback-in-dohlc i)))]
            [low (dohlc-low (stream-first (bull-pullback-in-dohlc i)))]
            [close (dohlc-close (stream-first (bull-pullback-in-dohlc i)))]
            [sma-20 (dv-value (stream-first (bull-pullback-in-sma-20 i)))]
            [sma-20-slope (dv-value (stream-first (bull-pullback-in-sma-20-slope i)))]
            [sma-50 (dv-value (stream-first (bull-pullback-in-sma-50 i)))]
            [sma-50-slope (dv-value (stream-first (bull-pullback-in-sma-50-slope i)))]
            [satr (dv-value (stream-first (bull-pullback-in-satr-50 i)))]
            [dc-20-high (dv-value (stream-first (bull-pullback-in-dc-20-high i)))]
            [dc-20-low (dv-value (stream-first (bull-pullback-in-dc-20-low i)))])
        ; (displayln t)
        ; (displayln p)
        ; (displayln h)
        ; (printf "~a ~a ~a ~a ~a ~a ~a ~a ~a ~a\n" date open high low close sma-20 sma-50 satr dc-high dc-low)
        (cond
          ; found satisfactory conditions for entry for the first time
          [(and (null? t)
                (null? p)
                (< satr (* close 4/100))
                (> sma-20 sma-50)
                (< 0 sma-50-slope)
                (> close sma-20)
                (< 0 sma-20-slope)
                (> close open)
                (> (- dc-20-high satr) high)
                (> close sma-20 open)
                (> (- sma-20 sma-50) (- dc-20-high sma-20))
                ; (or (and (> close sma-20 open))
                ;     (and (> close sma-50 open)))
                )
           (let ([new-test (test 20
                                 (+ high 5/100)
                                 (- high (* satr 2))
                                 (+ high (* satr 4)))])
             (bull-pullback new-test
                            p
                            (history (append (history-test h)
                                             (list (dv date new-test)))
                                     (history-trade h))
                            (bull-pullback-in-drop-1 i)))]
          ; satisfactory conditions no longer exist for entry
          [(and (not (null? t))
                (null? p)
                (or (<= open (test-stop t))
                    (<= high (test-stop t))
                    (<= low (test-stop t))
                    (<= close (test-stop t))))
           (bull-pullback null p h (bull-pullback-in-drop-1 i))]
          ; satisfactory conditions exist for entry and open price leads to execution
          [(and (not (null? t))
                (null? p)
                (>= open (test-entry t)))
           (bull-pullback t (position open 1)
                          (history (history-test h)
                                   (append (history-trade h) (list (trade date open 1 t))))
                          (bull-pullback-in-drop-1 i))]
          ; satisfactory conditions exist for entry and price range leads to execution
          [(and (not (null? t))
                (null? p)
                (<= open (test-entry t))
                (>= high (test-entry t))
                (<= low (test-entry t)))
           (bull-pullback t
                          (position (test-entry t) 1)
                          (history (history-test h)
                                   (append (history-trade h) (list (trade date (test-entry t) 1 t))))
                          (bull-pullback-in-drop-1 i))]
          ; have position and open below stop
          [(and (not (null? p))
                (<= open (test-stop t)))
           (bull-pullback null null
                          (history (history-test h)
                                   (append (history-trade h) (list (trade date open -1 t))))
                          (bull-pullback-in-drop-1 i))]
          ; have position and price range above stop
          [(and (not (null? p))
                (> open (test-stop t))
                (>= high (test-stop t))
                (<= low (test-stop t)))
           (bull-pullback null null
                          (history (history-test h)
                                   (append (history-trade h) (list (trade date (test-stop t) -1 t))))
                          (bull-pullback-in-drop-1 i))]
          ; have position and both parts of open/close above target and stop
          [(and (not (null? p))
                (> open (test-target t))
                (> close (test-target t))
                (> open (test-stop t))
                (> close (test-stop t)))
           (let ([new-test (test (test-timeframe t)
                                 (test-entry t)
                                 (min open close)
                                 (test-target t))])
             (bull-pullback new-test
                            p
                            (history (append (history-test h)
                                             (list (dv date new-test)))
                                     (history-trade h))
                            (bull-pullback-in-drop-1 i)))]
          ; have position and timeframe has ended
          [(and (not (null? p))
                (= 0 (test-timeframe t)))
           (bull-pullback null null
                          (history (history-test h)
                                   (append (history-trade h) (list (trade date close -1 t))))
                          (bull-pullback-in-drop-1 i))]
          ; have position and should move stop closer to close
          [(and (not (null? p))
                (< (* 3 satr) (- low (test-stop t))))
           (let ([new-test (test (- (test-timeframe t) 1)
                                 (test-entry t)
                                 (+ (test-stop t) satr)
                                 (test-target t))])
             (bull-pullback new-test
                            p
                            (history (append (history-test h)
                                             (list (dv date new-test)))
                                     (history-trade h))
                            (bull-pullback-in-drop-1 i)))]
          ; have position and can do nothing
          [(not (null? p))
           (bull-pullback (test-timeframe-minus-1 t) p h (bull-pullback-in-drop-1 i))]
          ; have no position and can do nothing
          [else (bull-pullback t p h (bull-pullback-in-drop-1 i))]))))

(define (bull-pullback-execution dohlc-list)
  (let*-values ([(dohlc-vector) (list->vector dohlc-list)]
                [(sma-20) (simple-moving-average dohlc-vector 20)]
                [(sma-20-slope) (delta sma-20 50)]
                [(sma-50) (simple-moving-average dohlc-vector 50)]
                [(sma-50-slope) (delta sma-50 50)]
                [(satr-50) (simple-average-true-range dohlc-vector 50)]
                [(dc-20-high dc-20-low) (donchian-channel dohlc-vector 20)]
                [(min-length) (min (vector-length dohlc-vector)
                                   (vector-length sma-20)
                                   (vector-length sma-20-slope)
                                   (vector-length sma-50)
                                   (vector-length sma-50-slope)
                                   (vector-length satr-50)
                                   (vector-length dc-20-high)
                                   (vector-length dc-20-low))])
    (bull-pullback null null
                   (history (list) (list))
                   (bull-pullback-in (sequence->stream (vector-take-right dohlc-vector min-length))
                                     (sequence->stream (vector-take-right sma-20 min-length))
                                     (sequence->stream (vector-take-right sma-20-slope min-length))
                                     (sequence->stream (vector-take-right sma-50 min-length))
                                     (sequence->stream (vector-take-right sma-50-slope min-length))
                                     (sequence->stream (vector-take-right satr-50 min-length))
                                     (sequence->stream (vector-take-right dc-20-high min-length))
                                     (sequence->stream (vector-take-right dc-20-low min-length))))))
