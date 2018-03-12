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

(define (qap-goodness-permutation-diff solution-vector flow-matrix distance-matrix . indexes)
  (define goodness-diff 0)
  (define size (vector-length solution-vector))
  (define permutated-solution (make-vector size 0))
  (vector-copy! permutated-solution 0 solution-vector)
  (apply permute! (cons permutated-solution indexes))
  (define permutated-indexes (flatten indexes))
  (define counted-indexes (make-vector size #f))
  (for ([unit-p permutated-indexes] #:unless (vector-ref counted-indexes unit-p))
    (vector-set! counted-indexes unit-p #t)
    (define location-p (vector-ref solution-vector unit-p))
    (define location-p* (vector-ref permutated-solution unit-p))
      (for ([unit-k size] [location-k solution-vector] [location-k* permutated-solution]
            #:unless (or (vector-ref counted-indexes unit-k) (= unit-k unit-p) (and (= location-k location-p*) (= location-k* location-p))))
            (displayln (format "+ f(~a,~a)*(d(~a,~a)-d(~a,~a))" unit-p unit-k location-p* location-k location-p location-k))
            (set! goodness-diff (+ goodness-diff
                                   (* (matrix-ref flow-matrix unit-p unit-k)
                                      (- (matrix-ref distance-matrix location-p* location-k*)
                                         (matrix-ref distance-matrix location-p location-k)))))))
  (* 2 goodness-diff))

(define/match (permute! solution-vector . indexes)
  [(sol '()) sol]
  [(sol indexes)
   (match-define (list i j) (first indexes))
   (define aux (vector-ref sol i))
   (vector-set! sol i (vector-ref sol j)) 
   (vector-set! sol j aux)
   (apply permute! (cons sol (rest indexes)))])

