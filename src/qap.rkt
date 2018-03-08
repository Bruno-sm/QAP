#lang racket

(require racket/cmdline)
(require "data-reader.rkt" "goodness.rkt")
(require "greedy.rkt" "local-search.rkt")


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
    [("--greedy") "Uses the greedy algorithm" 
           (set! mode 'greedy)]
    [("--local-search-bf") "Uses the local search algorithm with best-first selection"
           (set! mode 'local-bf)]
    [("--local-search-bn") "Uses the local search algorithm with best neighbor selection"
           (set! mode 'local-bn)]
    #:args (file . more-files) (set! files (cons file more-files)))
  (set! files (map (lambda (f) (build-path (current-directory) f)) files))  
  (cond
    [(symbol=? mode 'greedy)
     (for-each (lambda (file) (print-solution greedy file)) files)]
    [(symbol=? mode 'local-bf)
     (for-each (lambda (file)
                 (print-solution local-search
                                 file
                                 `(,max-iterations ,best-first-dlb-selection)))
               files)]
    [(symbol=? mode 'local-bn)
     (for-each (lambda (file)
                 (print-solution local-search
                                 file
                                 `(,max-iterations ,best-neighbor-selection)))
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
  (displayln (format "goodness: ~a" (qap-goodness solution fm dm)))
  (displayln (format "best solution: ~a" opt-solution))
  (displayln (format "best goodness: ~a"
                     (qap-goodness (map (lambda (x) (sub1 x)) opt-solution) fm dm)))
  (displayln (format "cpu mlliseconds: ~a" cpu_ms))
  (displayln (format "real mlliseconds: ~a" real_ms))
  (displayln (format "garbage collection mlliseconds: ~a" gc_ms))
  (displayln ""))


(main)
