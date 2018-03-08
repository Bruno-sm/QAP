#lang racket

(provide local-search best-first-dlb-selection best-neighbor-selection) 

(require "goodness.rkt" "vector.rkt")


(define (local-search size flow-matrix distance-matrix max-iterations select-neighbor)
  (define solution-vector (vector-shuffle (build-vector size (lambda (i) i))))
  (define is-local-minimum #f)
  (define select-neighbor-fun! (first select-neighbor))
  (define select-neighbor-extra-args-builder (second select-neighbor))
  (define select-neighbor-extra-args (select-neighbor-extra-args-builder size))
  (define gpd (lambda (s i j)
                (qap-goodness-permutation-diff s i j flow-matrix distance-matrix)))
  (for ([i max-iterations] #:break is-local-minimum)
    (define select-neighbor-args (append `(,solution-vector ,gpd) select-neighbor-extra-args))
    (set! is-local-minimum (apply select-neighbor-fun! select-neighbor-args)))
  (vector->list solution-vector))

(define best-first-dlb-selection
  (list (lambda (solution-vector goodness-permutation-diff dlb-vector)
;          (displayln (format "Searching first better neighbor for ~a" solution-vector))
          (define improves #f)
          (define size (vector-length solution-vector))
          (for ([i size] #:unless (vector-ref dlb-vector i) #:break improves) 
            (for ([j size] #:unless (= i j) #:break improves)
              (define g-diff (goodness-permutation-diff solution-vector i j))
;              (displayln (format "Goodness diff permutation ~a: ~a" `(,i ,j) g-diff))
              (cond [(< g-diff 0)
                     (permute! solution-vector i j)
;                     (displayln (format "Better neighbor ~a" solution-vector))
                     (vector-set! dlb-vector i #f)
                     (vector-set! dlb-vector j #f)
                     (set! improves #t)]))
            (cond [(not improves) (vector-set! dlb-vector i #t)]))
          (not improves))
        ; Builds extra parameters for best-first-dlb-selection
        (lambda (size) (list (make-vector size #f)))))

(define best-neighbor-selection
  (list (lambda (solution-vector goodness-permutation-diff)
;          (displayln (format "Searching best neighbor for ~a" solution-vector))
          (define best-permutation '(0 0))
          (define best-permutation-diff 0)
          (define size (vector-length solution-vector))
          (for* ([i size] [j size] #:unless (= i j))
            (define g-diff (goodness-permutation-diff solution-vector i j))
;            (displayln (format "Goodness diff permutation ~a: ~a" `(,i ,j) g-diff))
            (cond [(< g-diff best-permutation-diff)
            (set! best-permutation (list i j))
            (set! best-permutation-diff g-diff)]))
          (cond [(< best-permutation-diff 0)
                 (permute! solution-vector (first best-permutation) (second best-permutation))
;                 (displayln (format "Best neighbor ~a" solution-vector))
                 #f]
                [else #t]))
        ; Builds extra parameters for best-neighbor-selection
        (lambda (size) empty)))

(define (permute! sol i j)
  (define aux (vector-ref sol i))
  (vector-set! sol i (vector-ref sol j)) 
  (vector-set! sol j aux))
