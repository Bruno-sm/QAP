#lang racket

(provide stm-tabu-search tabu-goodness-diff)

(require "qap-representation-utils.rkt" "vector.rkt")


(define (stm-tabu-search size flow-matrix distance-matrix max-iterations)
  (displayln "Starting tabu search")
  (define goodness-diff
          (lambda (s tr prev-diff prev-tr)
            (tabu-goodness-diff s tr prev-diff prev-tr flow-matrix distance-matrix)))
  (define solution (vector-shuffle (build-vector size (lambda (i) i))))
  (define goodness (qap-goodness (vector->list solution) flow-matrix distance-matrix))
  (define new-solution (vector-copy solution))
  (define prev-transposition empty)
  (define prev-diff (make-matrix size size '(0 #f)))
  (define tabu-hash (make-weak-hash))
  (define tabu-tenure (+ (* (random) (* 0.9 size)) (* 1.1 size)))
  (displayln (format "tabu tenure: ~a" tabu-tenure))
  (define change-tenure-counter (* 2 tabu-tenure))
  (define found-neighbour #t)
  (for ([i max-iterations] #:break (not found-neighbour)) 
    (cond [(= 0 change-tenure-counter)
           (set! tabu-tenure (+ (* (random) (* 0.9 size)) (* 1.1 size)))
           (displayln (format "tabu tenure: ~a" tabu-tenure))
           (set! change-tenure-counter (* 2 tabu-tenure))]
          [else (set! change-tenure-counter (sub1 change-tenure-counter))])
    (set!-values (found-neighbour new-solution prev-transposition prev-diff)
                 (tabu-selection new-solution
                                 tabu-hash
                                 i
                                 tabu-tenure
                                 prev-transposition
                                 prev-diff
                                 goodness-diff))
    (displayln (format "Stuck: ~a" (not found-neighbour)))
    (for ([unit size] [loc new-solution])
      (hash-set! tabu-hash (list unit loc) i)
      (displayln (format "Tabu-hash[~a]: ~a" (list unit loc) i)))
    (define new-goodness (qap-goodness (vector->list new-solution) flow-matrix distance-matrix))
    (cond [(< new-goodness goodness)
           (vector-copy! solution 0 new-solution)
           (set! goodness new-goodness)
           (displayln (format "New best solution: ~a (~a)" solution goodness))]))
  (vector->list solution))

(define (tabu-goodness-diff solution transposition prev-diff prev-transposition fm dm)
  (cond [(or (equal? prev-transposition empty)) 
         (qap-goodness-permutation-diff solution fm dm transposition)]
        [else (match-define (list u v) transposition)
              (match-define (list r s) prev-transposition)
              (cond [(or (= u r) (= u s) (= v r) (= v s)
                         (not (second (matrix-ref prev-diff u v))))
                     (qap-goodness-permutation-diff solution fm dm transposition)]
                    [else (let ([fvr (matrix-ref fm v r)] [fur (matrix-ref fm u r)]
                                [fvs (matrix-ref fm v s)] [fus (matrix-ref fm u s)]
                                [dur (matrix-ref dm (vector-ref solution u) (vector-ref solution r))]
                                [dvr (matrix-ref dm (vector-ref solution v) (vector-ref solution r))]
                                [dus (matrix-ref dm (vector-ref solution u) (vector-ref solution s))]
                                [dvs (matrix-ref dm (vector-ref solution v) (vector-ref solution s))])
                            (+ (first (matrix-ref prev-diff u v))
                               (* 2
                                  (+ fur (- fvr) fvs (- fus))
                                  (+ dus (- dvs) dvr (- dur)))))])]))

(define (tabu-selection solution tabu-hash iteration tabu-tenure prev-transposition prev-diff goodness-diff)
  (displayln (format "Searching best neighbor for ~a" solution))
  (define best-transposition '(0 0))
  (define best-transposition-diff 0)
  (define size (vector-length solution))
  (define diff (make-matrix size size '(0 #f)))
  (define found-neighbour #f)
  (for* ([i size] [j i]
         #:unless (or (< iteration (+ (hash-ref tabu-hash (list i (vector-ref solution j))
                                                (- tabu-tenure)) tabu-tenure))
                      (< iteration (+ (hash-ref tabu-hash (list j (vector-ref solution i))
                                                (- tabu-tenure)) tabu-tenure)))); TODO aspiration criterion 
    (matrix-set! diff i j (list (goodness-diff solution (list i j) prev-diff prev-transposition) #t))
    (displayln (format "Goodness diff permutation ~a: ~a" `(,i ,j) (first (matrix-ref diff i j))))
    (cond [(or (< (first (matrix-ref diff i j)) best-transposition-diff)
               (not found-neighbour))
           (set! found-neighbour #t)
           (set! best-transposition (list i j))
           (set! best-transposition-diff (first (matrix-ref diff i j)))]))
  (define new-solution (vector-copy solution))
  (permute! new-solution best-transposition)
  (displayln (format "Best neighbor ~a" new-solution))
  (values found-neighbour new-solution best-transposition diff))
