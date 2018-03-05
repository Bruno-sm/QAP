#lang racket

(require racket/cmdline)
(require "data-reader.rkt" "greedy.rkt" "local-search.rkt")


(define (main)
  (define max-iterations 50000)
  (define mode 'greedy)
  (define files empty)
  (command-line
    #:program "qap"
    #:once-each
    [("-m" "--max-iterations") mi
             "Maximum number of iterations"
             (set! max-iterations (string->number mi))]
    #:once-any
    [("-g" "--greedy") "Uses the greedy algorithm" 
           (set! mode 'greedy)]
    [("-l" "--local-search") "Uses the local search algorithm"
           (set! mode 'local)]
    #:args (file . more-files) (set! files (cons file more-files)))
  (set! files (map (lambda (f) (build-path (current-directory) f)) files))  
  (cond
    [(symbol=? mode 'greedy)
     (for-each (lambda (file) (print-solution greedy file)) files)]
    [(symbol=? mode 'local)
     (for-each (lambda (file)
                 (print-solution local-search file `(,max-iterations)))
               files)]))

(define (print-solution algorithm file [args empty])
  (match-define-values (size fm dm) (read-dat file))
  (match-define-values (ret cpu_ms real_ms gc_ms)
                       (time-apply algorithm (append (list size fm dm) args)))
  (define solution (first ret))
  (match-define-values (_ opt-goodness opt-solution)
                       (read-sln (path-replace-extension file #".sln")))
  (displayln (format "file: ~a" (path->string (file-name-from-path file))))
  (displayln (format "size: ~a" size))
  (displayln (format "solution: ~a" (map (lambda (x) (add1 x)) solution)))
  (displayln (format "goodness: ~a" (solution-goodness solution fm dm)))
  (displayln (format "best solution: ~a" opt-solution))
  (displayln (format "best goodness: ~a"
                     (solution-goodness (map (lambda (x) (sub1 x)) opt-solution) fm dm)))
  (displayln (format "cpu mlliseconds: ~a" cpu_ms))
  (displayln (format "real mlliseconds: ~a" real_ms))
  (displayln (format "garbage collection mlliseconds: ~a" gc_ms))
  (displayln ""))

(define (solution-goodness solution flow-matrix distance-matrix)
  (define goodness 0)
  (define n (length solution))
  (for ([location-i solution] [unit-i n])
    (for ([location-j solution] [unit-j n])
      (set! goodness
            (+ goodness
               (* (vector-ref (vector-ref distance-matrix location-i) location-j)
                  (vector-ref (vector-ref flow-matrix unit-i) unit-j))))))
  goodness)

(main)
