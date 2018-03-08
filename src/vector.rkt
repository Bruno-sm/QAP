#lang racket


(provide vector-foldl vector-shuffle)
(provide make-matrix matrix-ref matrix-set!)


(define (vector-foldl proc ini vec)
  (define val ini)
  (define n (vector-length vec))
  (for ([i n])
    (set! val (proc val (vector-ref vec i))))
  val)

(define (vector-shuffle vec)
  (list->vector (shuffle (vector->list vec))))

(define (make-matrix n m [val 0])
  (define matrix (make-vector n))
  (for ([i n]) (vector-set! matrix i (make-vector m 0)))
  matrix)

(define (matrix-ref matrix i j)
  (vector-ref (vector-ref matrix j) i))

(define (matrix-set! matrix i j val)
  (vector-set! (vector-ref matrix i) j val))
