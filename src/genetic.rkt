#lang racket

(require "genetic-operators.rkt" "qap-representation-utils.rkt"
         "vector.rkt" "local-search.rkt")

(provide genetic-generational genetic-steady memetic-all memetic-rand memetic-best)


(define (genetic-generational size flow-matrix distance-matrix max-eval crossover population-size cross-prob mut-prob)
  (define goodness-fun (lambda (x)
                         (qap-goodness (vector->list x) flow-matrix distance-matrix)))
  (define population (build-vector population-size
                                 (lambda (i)
                                   (let ([ind (vector-shuffle
                                                (build-vector size (lambda (i) i)))])
                                     (list ind (goodness-fun ind))))))
  (define best (vector-argmin second population))
  (displayln (format "Generated initial generation of ~a individuals"
                     (vector-length population)))
  (define evaluations population-size)
  (define cross-num (exact-round (* cross-prob (/ population-size 2))))
  (define mut-num (exact-round (* mut-prob population-size size)))
  (for ([n (in-naturals)] #:break (> evaluations max-eval))
    (displayln (format "Generation ~a:" n))
    (displayln (format "  ~a/~a evaluations" evaluations max-eval))
    (displayln (format "  Best: ~a" best))
    (displayln (format "  Best goodnes: ~a" (goodness-fun (first best))))
    (displayln (format "  Selecting ~a individuals" population-size))
    (set! population
      (for/vector ([i population])
        (tournament-selection population second)))
    (displayln (format "  Crossing ~a individuals" cross-num))
    (for ([i (in-range 0 (* 2 cross-num) 2)])
        (define-values (c1 c2) (crossover (first (vector-ref population i))
                                          (first (vector-ref population (add1 i)))))
        (vector-set! population i (list c1 (goodness-fun c1)))
        (vector-set! population (add1 i) (list c2 (goodness-fun c2))))
    (displayln (format "  Generated child population of ~a individuals"
                       population-size))
    (displayln (format "  Doing ~a mutations" mut-num))
    (for ([i mut-num])
      (define ind (random population-size))
      (define mut (mutate (first (vector-ref population ind)) (random size)))
      (vector-set! population ind (list mut (goodness-fun mut))))
    (vector-set! population
                 (vector-index-max population second)
                 best)
    (set! evaluations (+ evaluations cross-num mut-num))
    (set! best (vector-argmin second population)))
  (vector->list (first best)))

(define (genetic-steady size flow-matrix distance-matrix max-eval crossover population-size mut-prob)
  (define goodness-fun (lambda (x)
                         (qap-goodness (vector->list x) flow-matrix distance-matrix)))
  (define population (build-vector population-size
                                 (lambda (i)
                                   (let ([ind (vector-shuffle
                                                (build-vector size (lambda (i) i)))])
                                     (list ind (goodness-fun ind))))))
  (displayln (format "Generated initial generation of ~a individuals"
                     population-size))
  (define evaluations population-size)
  (define mut-num (exact-round (* mut-prob population-size size)))
  (for ([n (in-naturals)] #:break (> evaluations max-eval))
    (displayln (format "Generation ~a:" n))
    (displayln (format "  ~a/~a evaluations" evaluations max-eval))
    (displayln (format "  Selecting 2 parents"))
    (define parent1 (tournament-selection population second))
    (define parent2 (tournament-selection population second))
    (displayln (format "  Crossing 2 individuals"))
    (define-values (child1 child2) (crossover (first parent1) (first parent2)))
    (displayln (format "  Doing ~a mutations" mut-num))
    (for ([i mut-num])
      (define ind (random 2))
      (cond [(= ind 0) (set! child1 (mutate child1 (random size)))]
            [else (set! child2 (mutate child2 (random size)))]))
    (define ind-worst (vector-index-max population second))
    (vector-set! population ind-worst (list -1 -1))
    (vector-set! population
                 (vector-index-max population second)
                 (list child1 (goodness-fun child1)))
    (vector-set! population ind-worst (list child2 (goodness-fun child2)))
    (set! evaluations (+ evaluations 2)))
  (vector->list (first (vector-argmin second population))))

(define (memetic-all size flow-matrix distance-matrix max-eval crossover population-size cross-prob mut-prob ls-gen)
  (define goodness-fun (lambda (x)
                         (qap-goodness (vector->list x) flow-matrix distance-matrix)))
  (define population (build-vector population-size
                                 (lambda (i)
                                   (let ([ind (vector-shuffle
                                                (build-vector size (lambda (i) i)))])
                                     (list ind (goodness-fun ind))))))
  (define best (vector-argmin second population))
  (displayln (format "Generated initial generation of ~a individuals"
                     (vector-length population)))
  (define evaluations population-size)
  (define cross-num (exact-round (* cross-prob (/ population-size 2))))
  (define mut-num (exact-round (* mut-prob population-size size)))
  (for ([n (in-naturals)] #:break (> evaluations max-eval))
    (displayln (format "Generation ~a:" n))
    (cond [(= (modulo n ls-gen) 0)
           (define ev (vector 0))
           (displayln "Starting local search")
           (for ([i population-size])
             (vector-set! population i
                          (parameterize ([current-output-port (open-output-nowhere)])
                            (let ([res (list->vector
                                         (local-search size flow-matrix
                                                       distance-matrix 400
                                                       best-first-dlb-selection
                                                       (first (vector-ref population i))
                                                       ev))])
                              (list res (goodness-fun res)))))
           (set! evaluations (+ evaluations (min (vector-ref ev 0) 400))))])
    (displayln (format "  ~a/~a evaluations" evaluations max-eval))
    (displayln (format "  Best: ~a" best))
    (displayln (format "  Best goodnes: ~a" (goodness-fun (first best))))
    (displayln (format "  Selecting ~a individuals" population-size))
    (set! population
      (for/vector ([i population])
        (tournament-selection population second)))
    (displayln (format "  Crossing ~a individuals" cross-num))
    (for ([i (in-range 0 (* 2 cross-num) 2)])
        (define-values (c1 c2) (crossover (first (vector-ref population i))
                                          (first (vector-ref population (add1 i)))))
        (vector-set! population i (list c1 (goodness-fun c1)))
        (vector-set! population (add1 i) (list c2 (goodness-fun c2))))
    (displayln (format "  Generated child population of ~a individuals"
                       population-size))
    (displayln (format "  Doing ~a mutations" mut-num))
    (for ([i mut-num])
      (define ind (random population-size))
      (define mut (mutate (first (vector-ref population ind)) (random size)))
      (vector-set! population ind (list mut (goodness-fun mut))))
    (vector-set! population
                 (vector-index-max population second)
                 best)
    (set! evaluations (+ evaluations cross-num mut-num))
    (set! best (vector-argmin second population)))
  (vector->list (first best)))

(define (memetic-rand size flow-matrix distance-matrix max-eval crossover population-size cross-prob mut-prob ls-gen)
  (define goodness-fun (lambda (x)
                         (qap-goodness (vector->list x) flow-matrix distance-matrix)))
  (define population (build-vector population-size
                                 (lambda (i)
                                   (let ([ind (vector-shuffle
                                                (build-vector size (lambda (i) i)))])
                                     (list ind (goodness-fun ind))))))
  (define best (vector-argmin second population))
  (displayln (format "Generated initial generation of ~a individuals"
                     (vector-length population)))
  (define evaluations population-size)
  (define cross-num (exact-round (* cross-prob (/ population-size 2))))
  (define mut-num (exact-round (* mut-prob population-size size)))
  (define ls-num (exact-round (* 0.1 population-size)))
  (for ([n (in-naturals)] #:break (> evaluations max-eval))
    (displayln (format "Generation ~a:" n))
    (cond [(= (modulo n ls-gen) 0)
           (define ev (vector 0))
           (displayln "Starting local search")
           (for ([i ls-num])
             (vector-set! population i
                          (parameterize ([current-output-port (open-output-nowhere)])
                            (let ([res (list->vector
                                         (local-search size flow-matrix
                                                       distance-matrix 400
                                                       best-first-dlb-selection
                                                       (first (vector-ref population i))
                                                       ev))])
                              (list res (goodness-fun res)))))
           (set! evaluations (+ evaluations (min (vector-ref ev 0) 400))))])
    (displayln (format "  ~a/~a evaluations" evaluations max-eval))
    (displayln (format "  Best: ~a" best))
    (displayln (format "  Best goodnes: ~a" (goodness-fun (first best))))
    (displayln (format "  Selecting ~a individuals" population-size))
    (set! population
      (for/vector ([i population])
        (tournament-selection population second)))
    (displayln (format "  Crossing ~a individuals" cross-num))
    (for ([i (in-range 0 (* 2 cross-num) 2)])
        (define-values (c1 c2) (crossover (first (vector-ref population i))
                                          (first (vector-ref population (add1 i)))))
        (vector-set! population i (list c1 (goodness-fun c1)))
        (vector-set! population (add1 i) (list c2 (goodness-fun c2))))
    (displayln (format "  Generated child population of ~a individuals"
                       population-size))
    (displayln (format "  Doing ~a mutations" mut-num))
    (for ([i mut-num])
      (define ind (random population-size))
      (define mut (mutate (first (vector-ref population ind)) (random size)))
      (vector-set! population ind (list mut (goodness-fun mut))))
    (vector-set! population
                 (vector-index-max population second)
                 best)
    (set! evaluations (+ evaluations cross-num mut-num))
    (set! best (vector-argmin second population)))
  (vector->list (first best)))

(define (memetic-best size flow-matrix distance-matrix max-eval crossover population-size cross-prob mut-prob ls-gen)
  (define goodness-fun (lambda (x)
                         (qap-goodness (vector->list x) flow-matrix distance-matrix)))
  (define population (build-vector population-size
                                 (lambda (i)
                                   (let ([ind (vector-shuffle
                                                (build-vector size (lambda (i) i)))])
                                     (list ind (goodness-fun ind))))))
  (define best (vector-argmin second population))
  (displayln (format "Generated initial generation of ~a individuals"
                     (vector-length population)))
  (define evaluations population-size)
  (define cross-num (exact-round (* cross-prob (/ population-size 2))))
  (define mut-num (exact-round (* mut-prob population-size size)))
  (define ls-num (exact-round (* 0.1 population-size)))
  (for ([n (in-naturals)] #:break (> evaluations max-eval))
    (displayln (format "Generation ~a:" n))
    (cond [(= (modulo n ls-gen) 0)
           (define ev (vector 0))
           (vector-sort! population (lambda (a b) (< (second a) (second b))))
           (displayln "Starting local search")
           (for ([i ls-num])
             (vector-set! population i
                          (parameterize ([current-output-port (open-output-nowhere)])
                            (let ([res (list->vector
                                         (local-search size flow-matrix
                                                       distance-matrix 400
                                                       best-first-dlb-selection
                                                       (first (vector-ref population i))
                                                       ev))])
                              (list res (goodness-fun res)))))
           (set! evaluations (+ evaluations (min (vector-ref ev 0) 400))))])
    (displayln (format "  ~a/~a evaluations" evaluations max-eval))
    (displayln (format "  Best: ~a" best))
    (displayln (format "  Best goodnes: ~a" (goodness-fun (first best))))
    (displayln (format "  Selecting ~a individuals" population-size))
    (set! population
      (for/vector ([i population])
        (tournament-selection population second)))
    (displayln (format "  Crossing ~a individuals" cross-num))
    (for ([i (in-range 0 (* 2 cross-num) 2)])
        (define-values (c1 c2) (crossover (first (vector-ref population i))
                                          (first (vector-ref population (add1 i)))))
        (vector-set! population i (list c1 (goodness-fun c1)))
        (vector-set! population (add1 i) (list c2 (goodness-fun c2))))
    (displayln (format "  Generated child population of ~a individuals"
                       population-size))
    (displayln (format "  Doing ~a mutations" mut-num))
    (for ([i mut-num])
      (define ind (random population-size))
      (define mut (mutate (first (vector-ref population ind)) (random size)))
      (vector-set! population ind (list mut (goodness-fun mut))))
    (vector-set! population
                 (vector-index-max population second)
                 best)
    (set! evaluations (+ evaluations cross-num mut-num))
    (set! best (vector-argmin second population)))
  (vector->list (first best)))
