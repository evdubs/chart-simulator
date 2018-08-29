#lang racket

(require "../structs.rkt"
         "../technical-indicators.rkt")

(provide (struct-out high-base-in)
         high-base-in-drop-1
         high-base
         high-base-execution)

(struct high-base-in
  (dohlc
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

(define (high-base t ; timeframe entry stop target
                   p ; position
                   h ; history
                   i) ; market data inputs
  (if (or (stream-empty? (high-base-in-dohlc i))
          (stream-empty? (high-base-in-sma-20 i))
          (stream-empty? (high-base-in-sma-20-slope i))
          (stream-empty? (high-base-in-sma-50 i))
          (stream-empty? (high-base-in-sma-50-slope i))
          (stream-empty? (high-base-in-satr-50 i))
          (stream-empty? (high-base-in-dc-10-high i))
          (stream-empty? (high-base-in-dc-10-low i))
          (stream-empty? (high-base-in-dc-50-high i))
          (stream-empty? (high-base-in-dc-50-low i)))
      h

      (let ([date (dohlc-date (stream-first (high-base-in-dohlc i)))]
            [open (dohlc-open (stream-first (high-base-in-dohlc i)))]
            [high (dohlc-high (stream-first (high-base-in-dohlc i)))]
            [low (dohlc-low (stream-first (high-base-in-dohlc i)))]
            [close (dohlc-close (stream-first (high-base-in-dohlc i)))]
            [sma-20 (dv-value (stream-first (high-base-in-sma-20 i)))]
            [sma-20-slope (dv-value (stream-first (high-base-in-sma-20-slope i)))]
            [sma-50 (dv-value (stream-first (high-base-in-sma-50 i)))]
            [sma-50-slope (dv-value (stream-first (high-base-in-sma-50-slope i)))]
            [satr (dv-value (stream-first (high-base-in-satr-50 i)))]
            [dc-10-high (dv-value (stream-first (high-base-in-dc-10-high i)))]
            [dc-10-low (dv-value (stream-first (high-base-in-dc-10-low i)))]
            [dc-50-high (dv-value (stream-first (high-base-in-dc-50-high i)))]
            [dc-50-low (dv-value (stream-first (high-base-in-dc-50-low i)))])
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
                (< (- dc-10-high dc-10-low) (* satr 4/2))
                (< dc-50-high (+ dc-10-high (/ satr 5))))
           (let ([new-test (test 20
                                 (+ dc-10-high 5/100)
                                 (- dc-10-high (* satr 2))
                                 (+ dc-10-high (* satr 4)))])
             (high-base new-test
                       p
                       (history (append (history-test h)
                                        (list (dv date new-test)))
                                (history-trade h))
                       (high-base-in-drop-1 i)))]
          ; satisfactory conditions no longer exist for entry
          [(and (not (null? t))
                (null? p)
                (or (<= open (test-stop t))
                    (<= high (test-stop t))
                    (<= low (test-stop t))
                    (<= close (test-stop t))))
           (high-base null p h (high-base-in-drop-1 i))]
          ; satisfactory conditions exist for entry and open price leads to execution
          [(and (not (null? t))
                (null? p)
                (>= open (test-entry t)))
           (high-base t (position open 1)
                     (history (history-test h)
                              (append (history-trade h) (list (trade date open 1 t))))
                     (high-base-in-drop-1 i))]
          ; satisfactory conditions exist for entry and price range leads to execution
          [(and (not (null? t))
                (null? p)
                (<= open (test-entry t))
                (>= high (test-entry t))
                (<= low (test-entry t)))
           (high-base t
                     (position (test-entry t) 1)
                     (history (history-test h)
                              (append (history-trade h) (list (trade date (test-entry t) 1 t))))
                     (high-base-in-drop-1 i))]
          ; have position and open below stop
          [(and (not (null? p))
                (<= open (test-stop t)))
           (high-base null null
                     (history (history-test h)
                              (append (history-trade h) (list (trade date open -1 t))))
                     (high-base-in-drop-1 i))]
          ; have position and price range above stop
          [(and (not (null? p))
                (> open (test-stop t))
                (>= high (test-stop t))
                (<= low (test-stop t)))
           (high-base null null
                     (history (history-test h)
                              (append (history-trade h) (list (trade date (test-stop t) -1 t))))
                     (high-base-in-drop-1 i))]
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
             (high-base new-test
                       p
                       (history (append (history-test h)
                                        (list (dv date new-test)))
                                (history-trade h))
                       (high-base-in-drop-1 i)))]
          ; have position and timeframe has ended
          [(and (not (null? p))
                (= 0 (test-timeframe t)))
           (high-base null null
                     (history (history-test h)
                              (append (history-trade h) (list (trade date close -1 t))))
                     (high-base-in-drop-1 i))]
          ; have position and should move stop closer to close
          [(and (not (null? p))
                (< (* 3 satr) (- low (test-stop t))))
           (let ([new-test (test (- (test-timeframe t) 1)
                                 (test-entry t)
                                 (+ (test-stop t) satr)
                                 (test-target t))])
             (high-base new-test
                       p
                       (history (append (history-test h)
                                        (list (dv date new-test)))
                                (history-trade h))
                       (high-base-in-drop-1 i)))]
          ; have position and can do nothing
          [(not (null? p))
           (high-base (test-timeframe-minus-1 t) p h (high-base-in-drop-1 i))]
          ; have no position and can do nothing
          [else (high-base t p h (high-base-in-drop-1 i))]))))

(define (high-base-execution dohlc-list)
  (let*-values ([(dohlc-vector) (list->vector dohlc-list)]
                [(sma-20) (simple-moving-average dohlc-vector 20)]
                [(sma-20-slope) (delta sma-20 50)]
                [(sma-50) (simple-moving-average dohlc-vector 50)]
                [(sma-50-slope) (delta sma-50 50)]
                [(satr-50) (simple-average-true-range dohlc-vector 50)]
                [(dc-10-high dc-10-low) (donchian-channel dohlc-vector 10)]
                [(dc-50-high dc-50-low) (donchian-channel dohlc-vector 50)]
                [(min-length) (min (vector-length dohlc-vector)
                                   (vector-length sma-20)
                                   (vector-length sma-20-slope)
                                   (vector-length sma-50)
                                   (vector-length sma-50-slope)
                                   (vector-length satr-50)
                                   (vector-length dc-10-high)
                                   (vector-length dc-10-low)
                                   (vector-length dc-50-high)
                                   (vector-length dc-50-low))])
    (high-base null null
              (history (list) (list))
              (high-base-in (sequence->stream (vector-take-right dohlc-vector min-length))
                           (sequence->stream (vector-take-right sma-20 min-length))
                           (sequence->stream (vector-take-right sma-20-slope min-length))
                           (sequence->stream (vector-take-right sma-50 min-length))
                           (sequence->stream (vector-take-right sma-50-slope min-length))
                           (sequence->stream (vector-take-right satr-50 min-length))
                           (sequence->stream (vector-take-right dc-10-high min-length))
                           (sequence->stream (vector-take-right dc-10-low min-length))
                           (sequence->stream (vector-take-right dc-50-high min-length))
                           (sequence->stream (vector-take-right dc-50-low min-length))))))
