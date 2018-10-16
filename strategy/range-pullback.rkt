#lang racket/base

(require racket/stream
         racket/vector
         "../structs.rkt"
         "../technical-indicators.rkt")

(provide range-pullback-execution)

(struct range-pullback-in
  (dohlc
   sma-50
   satr-50
   dc-25-high
   dc-25-low
   dc-50-high
   dc-50-low
   csr-4)
  #:transparent)

(define (range-pullback-in-drop-1 lbi)
  (range-pullback-in (stream-rest (range-pullback-in-dohlc lbi))
                (stream-rest (range-pullback-in-sma-50 lbi))
                (stream-rest (range-pullback-in-satr-50 lbi))
                (stream-rest (range-pullback-in-dc-25-high lbi))
                (stream-rest (range-pullback-in-dc-25-low lbi))
                (stream-rest (range-pullback-in-dc-50-high lbi))
                (stream-rest (range-pullback-in-dc-50-low lbi))
                (stream-rest (range-pullback-in-csr-4 lbi))))

(define (range-pullback t ; timeframe entry stop target
                   p ; position
                   h ; history
                   i) ; market data inputs
  (if (or (stream-empty? (range-pullback-in-dohlc i))
          (stream-empty? (range-pullback-in-sma-50 i))
          (stream-empty? (range-pullback-in-satr-50 i))
          (stream-empty? (range-pullback-in-dc-25-high i))
          (stream-empty? (range-pullback-in-dc-25-low i))
          (stream-empty? (range-pullback-in-dc-50-high i))
          (stream-empty? (range-pullback-in-dc-50-low i))
          (stream-empty? (range-pullback-in-csr-4 i)))
      h

      (let ([date (dohlc-date (stream-first (range-pullback-in-dohlc i)))]
            [open (dohlc-open (stream-first (range-pullback-in-dohlc i)))]
            [high (dohlc-high (stream-first (range-pullback-in-dohlc i)))]
            [low (dohlc-low (stream-first (range-pullback-in-dohlc i)))]
            [close (dohlc-close (stream-first (range-pullback-in-dohlc i)))]
            [sma-50 (dv-value (stream-first (range-pullback-in-sma-50 i)))]
            [satr (dv-value (stream-first (range-pullback-in-satr-50 i)))]
            [dc-25-high (dv-value (stream-first (range-pullback-in-dc-25-high i)))]
            [dc-25-low (dv-value (stream-first (range-pullback-in-dc-25-low i)))]
            [dc-50-high (dv-value (stream-first (range-pullback-in-dc-50-high i)))]
            [dc-50-low (dv-value (stream-first (range-pullback-in-dc-50-low i)))]
            [csr-4 (dv-value (stream-first (range-pullback-in-csr-4 i)))])
        ; (displayln t)
        ; (displayln p)
        ; (displayln h)
        ; (printf "~a ~a ~a ~a ~a ~a ~a ~a ~a ~a\n" date open high low close sma-20 sma-50 satr dc-high dc-low)
        (cond
          ; found satisfactory conditions for entry for the first time
          [(and (null? t)
                (null? p)
                (< satr (* close 4/100))
                (> sma-50 (+ dc-25-low satr))
                (< sma-50 (- dc-25-high satr))
                (> (- dc-25-high dc-25-low) (* satr 3))
                (< (- dc-25-high dc-25-low) (* satr 5))
                (< (abs (- dc-25-high dc-50-high)) (* close 2/100))
                (< (abs (- dc-25-low dc-50-low)) (* close 2/100))
                (> (- dc-25-high (* 5/2 satr)) high)
                (<= csr-4 -3))
           (let ([new-test (test 20
                                 (+ high 5/100)
                                 (- high (* satr 2))
                                 (+ high (* satr 4)))])
             (range-pullback new-test
                       p
                       (history (append (history-test h)
                                        (list (dv date new-test)))
                                (history-trade h))
                       (range-pullback-in-drop-1 i)))]
          ; satisfactory conditions no longer exist for entry
          [(and (not (null? t))
                (null? p)
                (or (<= open (test-stop t))
                    (<= high (test-stop t))
                    (<= low (test-stop t))
                    (<= close (test-stop t))))
           (range-pullback null p h (range-pullback-in-drop-1 i))]
          ; satisfactory conditions exist for entry and open price leads to execution
          [(and (not (null? t))
                (null? p)
                (>= open (test-entry t)))
           (range-pullback t (position open 1)
                     (history (history-test h)
                              (append (history-trade h) (list (trade date open 1 t))))
                     (range-pullback-in-drop-1 i))]
          ; satisfactory conditions exist for entry and price range leads to execution
          [(and (not (null? t))
                (null? p)
                (<= open (test-entry t))
                (>= high (test-entry t))
                (<= low (test-entry t)))
           (range-pullback t
                     (position (test-entry t) 1)
                     (history (history-test h)
                              (append (history-trade h) (list (trade date (test-entry t) 1 t))))
                     (range-pullback-in-drop-1 i))]
          ; have position and open below stop
          [(and (not (null? p))
                (<= open (test-stop t)))
           (range-pullback null null
                     (history (history-test h)
                              (append (history-trade h) (list (trade date open -1 t))))
                     (range-pullback-in-drop-1 i))]
          ; have position and price range above stop
          [(and (not (null? p))
                (> open (test-stop t))
                (>= high (test-stop t))
                (<= low (test-stop t)))
           (range-pullback null null
                     (history (history-test h)
                              (append (history-trade h) (list (trade date (test-stop t) -1 t))))
                     (range-pullback-in-drop-1 i))]
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
             (range-pullback new-test
                       p
                       (history (append (history-test h)
                                        (list (dv date new-test)))
                                (history-trade h))
                       (range-pullback-in-drop-1 i)))]
          ; have position and timeframe has ended
          [(and (not (null? p))
                (= 0 (test-timeframe t)))
           (range-pullback null null
                     (history (history-test h)
                              (append (history-trade h) (list (trade date close -1 t))))
                     (range-pullback-in-drop-1 i))]
          ; have position and should move stop closer to close
          [(and (not (null? p))
                (< (* 3 satr) (- low (test-stop t))))
           (let ([new-test (test (- (test-timeframe t) 1)
                                 (test-entry t)
                                 (+ (test-stop t) satr)
                                 (test-target t))])
             (range-pullback new-test
                       p
                       (history (append (history-test h)
                                        (list (dv date new-test)))
                                (history-trade h))
                       (range-pullback-in-drop-1 i)))]
          ; have position and can do nothing
          [(not (null? p))
           (range-pullback (test-timeframe-minus-1 t) p h (range-pullback-in-drop-1 i))]
          ; have no position and can do nothing
          [else (range-pullback t p h (range-pullback-in-drop-1 i))]))))

(define (range-pullback-execution dohlc-list)
  (let*-values ([(dohlc-vector) (list->vector dohlc-list)]
                [(sma-50) (simple-moving-average dohlc-vector 50)]
                [(satr-50) (simple-average-true-range dohlc-vector 50)]
                [(dc-25-high dc-25-low) (donchian-channel dohlc-vector 25)]
                [(dc-50-high dc-50-low) (donchian-channel dohlc-vector 50)]
                [(csr-4) (crow-soldier-reversal dohlc-vector 4)]
                [(min-length) (min (vector-length dohlc-vector)
                                   (vector-length sma-50)
                                   (vector-length satr-50)
                                   (vector-length dc-25-high)
                                   (vector-length dc-25-low)
                                   (vector-length dc-50-high)
                                   (vector-length dc-50-low)
                                   (vector-length csr-4))])
    (range-pullback null null
              (history (list) (list))
              (range-pullback-in (sequence->stream (vector-take-right dohlc-vector min-length))
                           (sequence->stream (vector-take-right sma-50 min-length))
                           (sequence->stream (vector-take-right satr-50 min-length))
                           (sequence->stream (vector-take-right dc-25-high min-length))
                           (sequence->stream (vector-take-right dc-25-low min-length))
                           (sequence->stream (vector-take-right dc-50-high min-length))
                           (sequence->stream (vector-take-right dc-50-low min-length))
                           (sequence->stream (vector-take-right csr-4 min-length))))))
