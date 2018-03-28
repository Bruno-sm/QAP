#lang racket

(require rackunit
         rackunit/text-ui
         "vector.rkt"
         "qap-representation-utils.rkt"
         "data-reader.rkt"
         "tabu-search.rkt")

(define qap-goodness-tests
  (test-suite
   "Tests for qap-goodness function"
   (let ([dat-files '("data/chr22a.dat" "data/wil50.dat" "data/wil100.dat"
                      "data/lipa60b.dat" "data/tai150b.dat" "data/sko81.dat"
                      "data/had20.dat" "data/esc128.dat" "data/tho40.dat")]
         [sln-files '("data/chr22a.sln" "data/wil50.sln" "data/wil100.sln"
                      "data/lipa60b.sln" "data/tai150b.sln" "data/sko81.sln"
                      "data/had20.sln" "data/esc128.sln" "data/tho40.sln")])
     (for ([df dat-files] [sf sln-files])
       (match-let-values ([(size fm dm) (read-dat df)]
                           [(_ goodness solution) (read-sln sf)])
         (check-equal? (qap-goodness (map (lambda (x) (sub1 x)) solution) fm dm)
                       goodness 
                       df))))))

(define qap-goodness-permutation-diff-tests
  (test-suite
   "Tests for qap-goodness-permutation-diff function"
   (match-let*-values ([(size fm dm) (read-dat "data/chr22a.dat")]
                       [(sol1) (build-vector size (lambda (i) i))])
     (for* ([i size] [j i])
       (let* ([trans (list i j)]
              [sol2 (vector-copy sol1)])
         (permute! sol2 trans)
         (check-equal? (qap-goodness-permutation-diff sol1 fm dm trans)
                       (- (qap-goodness (vector->list sol2) fm dm)
                          (qap-goodness (vector->list sol1) fm dm))
                       (format "sol1: ~a, sol2: ~a, tr: ~a" sol1 sol2 trans)))))))

(define tabu-goodness-diff-tests
  (test-suite
   "Tests for tabu-goodness-diff function"
   (match-let*-values ([(size fm dm) (read-dat "data/chr22a.dat")]
                       [(sol1) (build-vector size (lambda (i) i))])
     (for* ([r size] [s r] [u size] [v u])
       (let* ([trans1 (list r s)]
              [trans2 (list u v)]
              [prev-diff (qap-goodness-permutation-diff sol1 fm dm trans2)]
              [sol2 (vector-copy sol1)]
              [sol3 (vector-copy sol1)])
         (permute! sol2 trans1)
         (permute! sol3 trans1 trans2)
         (check-equal? (tabu-goodness-diff sol2 trans2 prev-diff trans1 fm dm)
                       (qap-goodness-permutation-diff sol2 fm dm trans2)
                       (format "sol1: ~a, sol2: ~a, sol3: ~a, tr1: ~a, tr2: ~a"
                               sol1 sol2 sol3 trans1 trans2)))))))

(run-tests qap-goodness-tests)
(run-tests qap-goodness-permutation-diff-tests)
;(run-tests tabu-goodness-diff-tests)
