#lang racket

(provide local-search best-first-dlb-selection best-neighbor-selection vnd-selection) 

(require "qap-representation-utils.rkt" "vector.rkt")


(define (local-search size flow-matrix distance-matrix max-iterations select-neighbor)
  (define solution-vector (vector-shuffle (build-vector size (lambda (i) i))))
  (define is-local-minimum #f)
  (define select-neighbor-fun! (first select-neighbor))
  (define select-neighbor-extra-args-builder (second select-neighbor))
  (define select-neighbor-extra-args (select-neighbor-extra-args-builder size))
  (define gpd (lambda (s . indexes)
                (apply qap-goodness-permutation-diff (append (list s flow-matrix distance-matrix) indexes))))
  (for ([i max-iterations] #:break is-local-minimum)
    (define select-neighbor-args (append `(,solution-vector ,gpd) select-neighbor-extra-args))
    (set! is-local-minimum (apply select-neighbor-fun! select-neighbor-args)))
  (vector->list solution-vector))

(define best-first-dlb-selection
  (list (lambda (solution-vector goodness-permutation-diff dlb-vector)
          (displayln (format "Searching first better neighbor for ~a" solution-vector))
          (define improves #f)
          (define size (vector-length solution-vector))
          (for ([i size] #:unless (vector-ref dlb-vector i) #:break improves) 
            (for ([j size] #:unless (= i j) #:break improves)
              (define g-diff (goodness-permutation-diff solution-vector (list i j)))
              (displayln (format "Goodness diff permutation ~a: ~a" `(,i ,j) g-diff))
              (cond [(< g-diff 0)
                     (permute! solution-vector (list i j))
                     (displayln (format "Better neighbor ~a" solution-vector))
                     (vector-set! dlb-vector i #f)
                     (vector-set! dlb-vector j #f)
                     (set! improves #t)]))
            (cond [(not improves) (vector-set! dlb-vector i #t)]))
          (not improves))
        ; Builds extra parameters for best-first-dlb-selection
        (lambda (size) (list (make-vector size #f)))))

(define best-neighbor-selection
  (list (lambda (solution-vector goodness-permutation-diff)
          (displayln (format "Searching best neighbor for ~a" solution-vector))
          (define best-permutation '(0 0))
          (define best-permutation-diff 0)
          (define size (vector-length solution-vector))
          (for* ([i size] [j i])
            (define g-diff (goodness-permutation-diff solution-vector (list i j)))
            (displayln (format "Goodness diff permutation ~a: ~a" `(,i ,j) g-diff))
            (cond [(< g-diff best-permutation-diff)
            (set! best-permutation (list i j))
            (set! best-permutation-diff g-diff)]))
          (cond [(< best-permutation-diff 0)
                 (permute! solution-vector best-permutation)
                 (displayln (format "Best neighbor ~a" solution-vector))
                 #f]
                [else #t]))
        ; Builds extra parameters for best-neighbor-selection
        (lambda (size) empty)))

(define vnd-selection
  (list (lambda (solution-vector goodness-permutation-diff dlb-vectors)
          (define is-local-max #t)
          (define size (vector-length solution-vector))
          (define neigh-gen! (vector-ref neighbor-structures 0))
          (for ([k 3] #:break (not is-local-max))
            (displayln (format "Using neigbourhood ~a" k))
            (set! neigh-gen! (vector-ref neighbor-structures k))
            (set! is-local-max 
              (neigh-gen! solution-vector goodness-permutation-diff (vector-ref dlb-vectors k)))
            (cond [is-local-max (vector-set! dlb-vectors k (make-vector size #f))]))
          is-local-max)
        (lambda (size) (list (vector (make-vector size #f) (make-vector size #f) (make-vector size #f))))))

; Neighbor structures for vnd
(define neighbor-structures
    ; Selects the first better neighbor with a transposition
  (list->vector
  `(,(lambda (sol goodness-permutation-diff dlb-vector)
       (define improves #f)
       (define size (vector-length sol))
       (for ([i size] #:unless (vector-ref dlb-vector i) #:break improves)
         (for ([j size] #:unless (= i j) #:break improves)
           (define g-diff (goodness-permutation-diff sol (list i j)))
           (cond [(< g-diff 0)
                  (permute! sol (list i j))
                  (vector-set! dlb-vector i #f)
                  (vector-set! dlb-vector j #f)
                  (set! improves #t)]))
         (cond [(not improves) (vector-set! dlb-vector i #t)]))
       (not improves))

    ; Selects the first better neighbor with three elements permutations
    ,(lambda (sol goodness-permutation-diff dlb-vector)
       (define improves #f)
       (define size (vector-length sol))
       (for ([i size] #:unless (vector-ref dlb-vector i) #:break (or improves (= i 90)))
         (for* ([j size] [k i] #:unless (or (= i j) (= k j)) #:break improves)
           (define g-diff
             (goodness-permutation-diff sol (list i j) (list k j)))
           (cond [(< g-diff 0)
                  (permute! sol (list i j) (list k j))
                  (vector-set! dlb-vector i #f)
                  (vector-set! dlb-vector j #f)
                  (vector-set! dlb-vector k #f)
                  (set! improves #t)]))
         (cond [(not improves) (vector-set! dlb-vector i #t)]))
       (not improves))

    ; Selects the first better neighbor with four elements permutations
    ,(lambda (sol goodness-permutation-diff dlb-vector)
       (define improves #f)
       (define size (vector-length sol))
       (for ([i size] #:unless (vector-ref dlb-vector i) #:break (or improves (= i 40)))
         (for* ([j i] [k i] [s k]
                #:unless (or (= j k) (= j s))
                #:break improves)
           (define g-diff
             (goodness-permutation-diff sol (list i j) (list k s)))
           (cond [(< g-diff 0)
                  (permute! sol (list i j) (list k s))
                  (vector-set! dlb-vector i #f)
                  (vector-set! dlb-vector j #f)
                  (vector-set! dlb-vector k #f)
                  (vector-set! dlb-vector s #f)
                  (set! improves #t)]))
         (cond [(not improves) (vector-set! dlb-vector i #t)]))
       (not improves)))))
