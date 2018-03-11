#lang racket


(provide qap-goodness qap-goodness-permutation-diff permute!)

(require "vector.rkt")


(define (qap-goodness solution flow-matrix distance-matrix)
  (define goodness 0)
  (define n (length solution))
  (for ([location-i solution] [unit-i n])
    (for ([location-j solution] [unit-j n])
      (set! goodness
            (+ goodness
               (* (matrix-ref distance-matrix location-i location-j)
                  (matrix-ref flow-matrix unit-i unit-j))))))
  goodness)

(define (qap-goodness-permutation-diff solution-vector i j flow-matrix distance-matrix)
  (define goodness-diff 0)
  (define location-i (vector-ref solution-vector i))
  (define location-j (vector-ref solution-vector j))
  (define size (vector-length solution-vector))
  (for ([unit-k size] [location-k solution-vector] #:unless (or (= unit-k i) (= unit-k j)))
    (set! goodness-diff (+ goodness-diff
                           (* (matrix-ref flow-matrix i unit-k)
                              (- (matrix-ref distance-matrix location-j location-k)
                                 (matrix-ref distance-matrix location-i location-k)))
                           (* (matrix-ref flow-matrix j unit-k)
                              (- (matrix-ref distance-matrix location-i location-k)
                                 (matrix-ref distance-matrix location-j location-k))))))
  (* 2 goodness-diff))

(define/match (permute! solution-vector . indexes)
  [(sol '()) sol]
  [(sol indexes)
   (match-define (list i j) (first indexes))
   (define aux (vector-ref sol i))
   (vector-set! sol i (vector-ref sol j)) 
   (vector-set! sol j aux)
   (apply permute! (cons sol (rest indexes)))])

