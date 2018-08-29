#lang racket

(require "../structs.rkt"
         "../technical-indicators.rkt")

(provide (struct-out ascending-triangle-in)
         ascending-triangle-in-drop-1
         ascending-triangle
         ascending-triangle-execution)

(struct ascending-triangle-in
  (dohlc
   sma-20
   sma-20-slope
   sma-50
   sma-50-slope
   satr-50
   dc-25-high
   dc-25-low
   dc-25-prev-high
   dc-25-prev-low)
  #:transparent)

(define (ascending-triangle-in-drop-1 atid)
  (ascending-triangle-in (stream-rest (ascending-triangle-in-dohlc atid))
                         (stream-rest (ascending-triangle-in-sma-20 atid))
                         (stream-rest (ascending-triangle-in-sma-20-slope atid))
                         (stream-rest (ascending-triangle-in-sma-50 atid))
                         (stream-rest (ascending-triangle-in-sma-50-slope atid))
                         (stream-rest (ascending-triangle-in-satr-50 atid))
                         (stream-rest (ascending-triangle-in-dc-25-high atid))
                         (stream-rest (ascending-triangle-in-dc-25-low atid))
                         (stream-rest (ascending-triangle-in-dc-25-prev-high atid))
                         (stream-rest (ascending-triangle-in-dc-25-prev-low atid))))

(define (ascending-triangle t ; timeframe entry stop target
                   p ; position
                   h ; history
                   i) ; market data inputs
  (if (or (stream-empty? (ascending-triangle-in-dohlc i))
          (stream-empty? (ascending-triangle-in-sma-20 i))
          (stream-empty? (ascending-triangle-in-sma-20-slope i))
          (stream-empty? (ascending-triangle-in-sma-50 i))
          (stream-empty? (ascending-triangle-in-sma-50-slope i))
          (stream-empty? (ascending-triangle-in-satr-50 i))
          (stream-empty? (ascending-triangle-in-dc-25-high i))
          (stream-empty? (ascending-triangle-in-dc-25-low i))
          (stream-empty? (ascending-triangle-in-dc-25-prev-high i))
          (stream-empty? (ascending-triangle-in-dc-25-prev-low i)))
      h

      (let ([date (dohlc-date (stream-first (ascending-triangle-in-dohlc i)))]
            [open (dohlc-open (stream-first (ascending-triangle-in-dohlc i)))]
            [high (dohlc-high (stream-first (ascending-triangle-in-dohlc i)))]
            [low (dohlc-low (stream-first (ascending-triangle-in-dohlc i)))]
            [close (dohlc-close (stream-first (ascending-triangle-in-dohlc i)))]
            [sma-20 (dv-value (stream-first (ascending-triangle-in-sma-20 i)))]
            [sma-20-slope (dv-value (stream-first (ascending-triangle-in-sma-20-slope i)))]
            [sma-50 (dv-value (stream-first (ascending-triangle-in-sma-50 i)))]
            [sma-50-slope (dv-value (stream-first (ascending-triangle-in-sma-50-slope i)))]
            [satr (dv-value (stream-first (ascending-triangle-in-satr-50 i)))]
            [dc-25-high (dv-value (stream-first (ascending-triangle-in-dc-25-high i)))]
            [dc-25-low (dv-value (stream-first (ascending-triangle-in-dc-25-low i)))]
            [dc-25-prev-high (dv-value (stream-first (ascending-triangle-in-dc-25-prev-high i)))]
            [dc-25-prev-low (dv-value (stream-first (ascending-triangle-in-dc-25-prev-low i)))])
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
                (> close (- dc-25-high (* satr 4/2)))
                (> dc-25-high dc-25-prev-high)
                (> dc-25-prev-high (* dc-25-high 99/100))
                (> (- dc-25-low (* (- dc-25-high dc-25-low) 1/2)) dc-25-prev-low))
                ; (< (- dc-25-high dc-25-low) (* satr 4/2))
                ; (< dc-25-prev-high (+ dc-25-high (/ satr 5))))
           (let ([new-test (test 20
                                 (+ dc-25-high 5/100)
                                 (- dc-25-high (* satr 2))
                                 (+ dc-25-high (* satr 4)))])
             (ascending-triangle new-test
                       p
                       (history (append (history-test h)
                                        (list (dv date new-test)))
                                (history-trade h))
                       (ascending-triangle-in-drop-1 i)))]
          ; satisfactory conditions no longer exist for entry
          [(and (not (null? t))
                (null? p)
                (or (<= open (test-stop t))
                    (<= high (test-stop t))
                    (<= low (test-stop t))
                    (<= close (test-stop t))))
           (ascending-triangle null p h (ascending-triangle-in-drop-1 i))]
          ; satisfactory conditions exist for entry and open price leads to execution
          [(and (not (null? t))
                (null? p)
                (>= open (test-entry t)))
           (ascending-triangle t (position open 1)
                     (history (history-test h)
                              (append (history-trade h) (list (trade date open 1 t))))
                     (ascending-triangle-in-drop-1 i))]
          ; satisfactory conditions exist for entry and price range leads to execution
          [(and (not (null? t))
                (null? p)
                (<= open (test-entry t))
                (>= high (test-entry t))
                (<= low (test-entry t)))
           (ascending-triangle t
                     (position (test-entry t) 1)
                     (history (history-test h)
                              (append (history-trade h) (list (trade date (test-entry t) 1 t))))
                     (ascending-triangle-in-drop-1 i))]
          ; have position and open below stop
          [(and (not (null? p))
                (<= open (test-stop t)))
           (ascending-triangle null null
                     (history (history-test h)
                              (append (history-trade h) (list (trade date open -1 t))))
                     (ascending-triangle-in-drop-1 i))]
          ; have position and price range above stop
          [(and (not (null? p))
                (> open (test-stop t))
                (>= high (test-stop t))
                (<= low (test-stop t)))
           (ascending-triangle null null
                     (history (history-test h)
                              (append (history-trade h) (list (trade date (test-stop t) -1 t))))
                     (ascending-triangle-in-drop-1 i))]
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
             (ascending-triangle new-test
                       p
                       (history (append (history-test h)
                                        (list (dv date new-test)))
                                (history-trade h))
                       (ascending-triangle-in-drop-1 i)))]
          ; have position and timeframe has ended
          [(and (not (null? p))
                (= 0 (test-timeframe t)))
           (ascending-triangle null null
                     (history (history-test h)
                              (append (history-trade h) (list (trade date close -1 t))))
                     (ascending-triangle-in-drop-1 i))]
          ; have position and should move stop closer to close
          [(and (not (null? p))
                (< (* 3 satr) (- low (test-stop t))))
           (let ([new-test (test (- (test-timeframe t) 1)
                                 (test-entry t)
                                 (+ (test-stop t) satr)
                                 (test-target t))])
             (ascending-triangle new-test
                       p
                       (history (append (history-test h)
                                        (list (dv date new-test)))
                                (history-trade h))
                       (ascending-triangle-in-drop-1 i)))]
          ; have position and can do nothing
          [(not (null? p))
           (ascending-triangle (test-timeframe-minus-1 t) p h (ascending-triangle-in-drop-1 i))]
          ; have no position and can do nothing
          [else (ascending-triangle t p h (ascending-triangle-in-drop-1 i))]))))

(define (ascending-triangle-execution dohlc-list)
  (let*-values ([(dohlc-vector) (list->vector dohlc-list)]
                [(sma-20) (simple-moving-average dohlc-vector 20)]
                [(sma-20-slope) (delta sma-20 50)]
                [(sma-50) (simple-moving-average dohlc-vector 50)]
                [(sma-50-slope) (delta sma-50 50)]
                [(satr-50) (simple-average-true-range dohlc-vector 50)]
                [(dc-25-high dc-25-low) (donchian-channel dohlc-vector 25)]
                [(dc-25-prev-high dc-25-prev-low) (values (shift dc-25-high 25) (shift dc-25-low 25))]
                [(min-length) (min (vector-length dohlc-vector)
                                   (vector-length sma-20)
                                   (vector-length sma-20-slope)
                                   (vector-length sma-50)
                                   (vector-length sma-50-slope)
                                   (vector-length satr-50)
                                   (vector-length dc-25-high)
                                   (vector-length dc-25-low)
                                   (vector-length dc-25-prev-high)
                                   (vector-length dc-25-prev-low))])
    (ascending-triangle null null
              (history (list) (list))
              (ascending-triangle-in (sequence->stream (vector-take-right dohlc-vector min-length))
                                     (sequence->stream (vector-take-right sma-20 min-length))
                                     (sequence->stream (vector-take-right sma-20-slope min-length))
                                     (sequence->stream (vector-take-right sma-50 min-length))
                                     (sequence->stream (vector-take-right sma-50-slope min-length))
                                     (sequence->stream (vector-take-right satr-50 min-length))
                                     (sequence->stream (vector-take-right dc-25-high min-length))
                                     (sequence->stream (vector-take-right dc-25-low min-length))
                                     (sequence->stream (vector-take-right dc-25-prev-high min-length))
                                     (sequence->stream (vector-take-right dc-25-prev-low min-length))))))
