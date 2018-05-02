#lang racket

(provide tournament-selection position-crossover pmx-crossover mutate)

(require "qap-representation-utils.rkt")


(define (tournament-selection population goodness-function)
  (define x1 (vector-ref population (random (vector-length population))))
  (define goodness-x1 (goodness-function x1))
  (define x2 (vector-ref population (random (vector-length population))))
  (define goodness-x2 (goodness-function x2))
  (cond [(< goodness-x1 goodness-x2) x1]
        [else x2]))

(define (position-crossover parent1 parent2)
  (define size (vector-length parent1))
  (define child (make-vector size 0))
  (define rest-indexes empty)
  (for ([i size])
    (cond [(= (vector-ref parent1 i) (vector-ref parent2 i))
           (vector-set! child i (vector-ref parent1 i))]
          [else (set! rest-indexes (cons i rest-indexes))]))
  (define shuffled-indexes (shuffle rest-indexes))
  (for ([i (in-list rest-indexes)] [si (in-list shuffled-indexes)])
    (vector-set! child i (vector-ref parent1 si)))
  (values child parent1))


(define (pmx-crossover parent1 parent2)
  (define p1 (vector->list parent1))
  (define p2 (vector->list parent2))
  (define size (vector-length parent1))
  (define left-bound (random (sub1 size)))
  (define right-bound (random left-bound size))
  (define l1 (take p1 left-bound))
  (define c1 (drop (take p1 right-bound) left-bound))
  (define r1 (take-right p1 (- size right-bound)))
  (define l2 (take p2 left-bound))
  (define c2 (drop (take p2 right-bound) left-bound))
  (define r2 (take-right p2 (- size right-bound)))
  (define relations1 (build-vector size (lambda(i) i)))
  (define relations2 (build-vector size (lambda(i) i)))
  (for ([i (in-range left-bound right-bound)])
    (vector-set! relations1 (vector-ref parent2 i) (vector-ref parent1 i))
    (vector-set! relations2 (vector-ref parent1 i) (vector-ref parent2 i)))
  (set! relations1 (simplify-relations relations1))
  (set! relations2 (simplify-relations relations2))
  (define child1 (append (map (lambda (x) (vector-ref relations1 x)) l1)
                         c2
                         (map (lambda (x) (vector-ref relations1 x)) r1)))
  (define child2 (append (map (lambda (x) (vector-ref relations2 x)) l2)
                         c1
                         (map (lambda (x) (vector-ref relations2 x)) r2)))
  (values (list->vector child1) (list->vector child2)))

(define (simplify-relations rel)
  (for/vector ([i rel])
    (define sim-rel i)
    (for ([j (vector-length rel)] #:break (= sim-rel (vector-ref rel sim-rel)))
      (set! sim-rel (vector-ref rel sim-rel)))
    sim-rel))

(define (mutate individual gen-index)
  (define gen2-index (random (vector-length individual)))
  (permute! individual (list gen-index gen2-index))
  individual)
