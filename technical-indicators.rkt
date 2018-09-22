#lang racket/base

(require racket/sequence
         racket/vector
         "structs.rkt")

(provide simple-moving-average
         simple-average-true-range
         donchian-channel
         delta
         shift)

(define (vector-partition v period step)
  (if (> period (vector-length v)) (vector)
      (vector-append (vector (vector-take v period))
                     (vector-partition (vector-drop v step) period step))))

(define (simple-moving-average date-ohlc-vector period)
  (vector-map (λ (v) (dv (dohlc-date (vector-ref v (- period 1)))
                         (/ (sequence-fold (λ (acc el) (+ acc (dohlc-close el))) 0
                                           (vector-take v period)) period)))
              (vector-partition date-ohlc-vector period 1)))

(define (true-range high low previous-close)
  (max (- high low)
       (abs (- high previous-close))
       (abs (- low previous-close))))

(define (simple-average-true-range date-ohlc-vector period)
  (let ([true-ranges (vector-map (λ (v) (dv (dohlc-date (vector-ref v 1))
                                            (true-range (dohlc-high (vector-ref v 1))
                                                        (dohlc-low (vector-ref v 1))
                                                        (dohlc-close (vector-ref v 0)))))
                                 (vector-partition date-ohlc-vector 2 1))])
    (vector-map (λ (v) (dv (dv-date (vector-ref v (- period 1)))
                           (/ (sequence-fold (λ (acc el) (+ acc (dv-value el))) 0
                                             (vector-take v period)) period)))
                (vector-partition true-ranges period 1))))

(define (donchian-channel date-ohlc-vector period)
  (values (vector-map (λ (v) (dv (dohlc-date (vector-ref v (- period 1)))
                                 (sequence-fold (λ (acc el) (max acc (dohlc-high el)))
                                                (dohlc-high (vector-ref v 0)) v)))
                      (vector-partition date-ohlc-vector period 1))
          (vector-map (λ (v) (dv (dohlc-date (vector-ref v (- period 1)))
                                 (sequence-fold (λ (acc el) (min acc (dohlc-low el)))
                                                (dohlc-low (vector-ref v 0)) v)))
                      (vector-partition date-ohlc-vector period 1))))

(define (delta dv-vector period)
  (vector-map (λ (v) (dv (dv-date (vector-ref v (- period 1)))
                         (- (dv-value (vector-ref v (- period 1))) (dv-value (vector-ref v 0)))))
              (vector-partition dv-vector period 1)))

(define (shift dv-vector period)
  (vector-map (λ (v) (dv (dv-date (vector-ref v (- period 1)))
                         (dv-value (vector-ref v 0))))
              (vector-partition dv-vector period 1)))
