#lang racket

(provide greedy)


(define (greedy size flow-matrix distance-matrix)
  (define potential-flows-vector (make-vector size '(0 0)))
  (for* ([i size] [j size])
    (vector-set! potential-flows-vector i
                 `(,i ,(+ (second (vector-ref potential-flows-vector i))
                          (vector-ref (vector-ref flow-matrix i) j)))))
  (vector-sort! potential-flows-vector (lambda (p1 p2) (> (second p1) (second p2))))
  (define potential-distances-vector (make-vector size '(0 0)))
  (for* ([i size] [j size])
    (vector-set! potential-distances-vector i
                 `(,i ,(+ (second (vector-ref potential-distances-vector i))
                          (vector-ref (vector-ref distance-matrix i) j)))))
  (vector-sort! potential-distances-vector (lambda (p1 p2) (< (second p1) (second p2))))
  (define solution-vector (make-vector size 0))
  (for ([i size])
    (vector-set! solution-vector
                 (first (vector-ref potential-flows-vector i))
                 (first (vector-ref potential-distances-vector i))))
  (vector->list solution-vector))
