#lang racket

(require racket/cmdline)
(require "data-reader.rkt" "goodness.rkt")
(require "greedy.rkt" "local-search.rkt")


(struct cli-options (algorithm
                     max-iterations
                     output-mode)
                    #:mutable)

(define (main)
  (define options (cli-options 'greedy 50000 'human-readable))
  (define files empty)
  (command-line
    #:program "qap"
    #:once-each
    [("-m" "--max-iterations") mi
             "Maximum number of iterations"
             (set-cli-options-max-iterations! options (string->number mi))]
    #:once-any
    [("--csv-output") "Displays the output in csv format"
             (set-cli-options-output-mode! options 'csv)]
    #:once-any
    [("--greedy") "Uses the greedy algorithm" 
           (set-cli-options-algorithm! options 'greedy)]
    [("--local-search-bf") "Uses the local search algorithm with best-first selection"
           (set-cli-options-algorithm! options 'local-bf)]
    [("--local-search-bn") "Uses the local search algorithm with best neighbor selection"
           (set-cli-options-algorithm! options 'local-bn)]
    #:args (file . more-files) (set! files (cons file more-files)))
  (set! files (map (lambda (f) (build-path (current-directory) f)) files))  
  (cond [(symbol=? (cli-options-output-mode options) 'csv)
         (displayln "Case,value,opt_value,cpu_ms,real_ms,gc_ms")])
  (cond
    [(symbol=? (cli-options-algorithm options) 'greedy)
     (for-each (lambda (file) (print-solution greedy file options)) files)]
    [(symbol=? (cli-options-algorithm options) 'local-bf)
     (for-each (lambda (file)
                 (print-solution local-search
                                 file
                                 options
                                 `(,(cli-options-max-iterations options)
                                   ,best-first-dlb-selection)))
               files)]
    [(symbol=? (cli-options-algorithm options) 'local-bn)
     (for-each (lambda (file)
                 (print-solution local-search
                                 file
                                 options
                                 `(,(cli-options-max-iterations options)
                                   ,best-neighbor-selection)))
               files)]))

(define (print-solution algorithm file cli-options [args empty])
  (match-define-values (size fm dm) (read-dat file))
  (match-define-values (ret cpu-ms real-ms gc-ms)
                       (time-apply algorithm (append (list size fm dm) args)))
  (define solution (first ret))
  (match-define-values (_ opt-goodness opt-solution)
                       (read-sln (path-replace-extension file #".sln")))
  (cond
    [(symbol=? (cli-options-output-mode cli-options) 'csv)
     (displayln (format "~a,~a,~a,~a,~a,~a"
                        (file-name-from-path file)
                        (qap-goodness solution fm dm) opt-goodness
                        cpu-ms real-ms gc-ms))]
    [else
     (displayln (format "file: ~a" (path->string (file-name-from-path file))))
     (displayln (format "size: ~a" size))
     (displayln (format "solution: ~a" (map (lambda (x) (add1 x)) solution)))
     (displayln (format "goodness: ~a" (qap-goodness solution fm dm)))
     (displayln (format "best solution: ~a" opt-solution))
     (displayln (format "best goodness: ~a"
                        (qap-goodness (map (lambda (x) (sub1 x)) opt-solution) fm dm)))
     (displayln (format "cpu mlliseconds: ~a" cpu-ms))
     (displayln (format "real mlliseconds: ~a" real-ms))
     (displayln (format "garbage collection mlliseconds: ~a" gc-ms))
     (displayln "")]))


(main)
