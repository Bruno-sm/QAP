#lang racket

(require racket/cmdline)
(require "data-reader.rkt" "qap-representation-utils.rkt")
(require "greedy.rkt" "local-search.rkt" "tabu-search.rkt")


(struct cli-options (algorithm
                     executions-number
                     max-iterations
                     output-mode)
                    #:mutable)

(define (main)
  (define options (cli-options 'greedy 1 50000 'human-readable))
  (define files empty)
  (command-line
    #:program "qap"
    #:once-each
    [("-m" "--max-iterations")
     mi
     "Maximum number of iterations inside an algorithm"
     (set-cli-options-max-iterations! options (string->number mi))]
    [("-r" "--repetitions")
     exec-number
     "Number of executions for each case. The result is the mean of all executions"
     (set-cli-options-executions-number! options (string->number exec-number))]
    #:once-any
    [("--csv-output")
     "Displays the output in csv format"
     (set-cli-options-output-mode! options 'csv)]
    [("--debug-output")
     "Displays the output in csv format"
     (set-cli-options-output-mode! options 'debug)]
    #:once-any
    [("--greedy")
     "Uses the greedy algorithm"
     (set-cli-options-algorithm! options 'greedy)]
    [("--local-search-bf")
     "Uses the local search algorithm with best-first selection"
     (set-cli-options-algorithm! options 'local-bf)]
    [("--local-search-bn")
     "Uses the local search algorithm with best neighbor selection"
     (set-cli-options-algorithm! options 'local-bn)]
    [("--local-search-vnd")
     "Uses the local search algorithm with variable neighbourhood descent"
     (set-cli-options-algorithm! options 'local-vnd)]
    [("--stm-tabu-search")
     "Uses the local search algorithm with variable neighbourhood descent"
     (set-cli-options-algorithm! options 'stm-tabu)]
    #:args (file . more-files) (set! files (cons file more-files)))
  (set! files (map (lambda (f) (build-path (current-directory) f)) files))  
  (cond [(symbol=? (cli-options-output-mode options) 'csv)
         (displayln "Case,size,repetitions,best_value,values_mean,values_deviation,opt_value,cpu_ms,real_ms,gc_ms")])
  (match-define-values (algorithm alg-args)
    (let ([alg (cli-options-algorithm options)])
      (cond
        [(symbol=? alg 'greedy) (values greedy empty)]
        [(symbol=? alg 'local-bf)
         (values local-search `(,(cli-options-max-iterations options)
                                ,best-first-dlb-selection))]
        [(symbol=? alg 'local-bn)
         (values local-search `(,(cli-options-max-iterations options)
                                ,best-neighbor-selection))]
        [(symbol=? alg 'local-vnd)
         (values local-search `(,(cli-options-max-iterations options)
                                ,vnd-selection))]
        [(symbol=? alg 'stm-tabu)
         (values stm-tabu-search `(,(cli-options-max-iterations options)))])))
  (for-each (lambda (file) (print-solution algorithm file options alg-args)) files))

(define (print-solution algorithm file cli-options [args empty])
  (match-define-values (size fm dm) (read-dat file))
  (match-define-values (_ opt-goodness opt-solution)
                       (read-sln (path-replace-extension file #".sln")))
  (define solution empty)
  (define solution-goodness -1)
  (define goodness-mean 0.0)
  (define cpu-ms 0.0)
  (define real-ms 0.0)
  (define gc-ms 0.0)
  (for ([i (cli-options-executions-number cli-options)])
    (define output-port (if (symbol=? (cli-options-output-mode cli-options) 'debug)
                            (current-output-port) (open-output-nowhere)))
    (match-define-values (ret cpu-ms* real-ms* gc-ms*)
                         (parameterize ([current-output-port output-port])
                           (time-apply algorithm (append (list size fm dm) args))))
    (set! cpu-ms (+ cpu-ms cpu-ms*))
    (set! real-ms (+ real-ms real-ms*))
    (set! gc-ms (+ gc-ms gc-ms*))
    (let* ([s (first ret)] [sg (qap-goodness s fm dm)])
      (set! goodness-mean (+ goodness-mean sg))
      (cond [(or (= -1 solution-goodness) (< sg solution-goodness))
             (set! solution s)
             (set! solution-goodness sg)])))
  (set! cpu-ms (/ cpu-ms (cli-options-executions-number cli-options)))
  (set! real-ms (/ real-ms (cli-options-executions-number cli-options)))
  (set! gc-ms (/ gc-ms (cli-options-executions-number cli-options)))
  (set! goodness-mean (/ goodness-mean (cli-options-executions-number cli-options)))
  (define goodness-deviation (/ (round (* 10000 (/ (- goodness-mean opt-goodness) opt-goodness)))
                                100))
  (cond
    [(symbol=? (cli-options-output-mode cli-options) 'csv)
     (displayln (format "~a,~a,~a,~a,~a,~a,~a,~a,~a,~a"
                        (file-name-from-path file)
                        size
                        (cli-options-executions-number cli-options)
                        solution-goodness goodness-mean goodness-deviation opt-goodness
                        cpu-ms real-ms gc-ms))]
    [else
     (displayln (format "file: ~a" (path->string (file-name-from-path file))))
     (displayln (format "size: ~a" size))
     (displayln (format "repetitions: ~a" (cli-options-executions-number cli-options)))
     (displayln (format "best solution: ~a" (map (lambda (x) (add1 x)) solution)))
     (displayln (format "best goodness: ~a" solution-goodness))
     (displayln (format "goodness mean: ~a" goodness-mean))
     (displayln (format "goodness deviation: ~a" goodness-deviation))
     (displayln (format "optimal solution: ~a" opt-solution))
     (displayln (format "optimal goodness: ~a" opt-goodness))
     (displayln (format "cpu mlliseconds mean: ~a" cpu-ms))
     (displayln (format "real mlliseconds mean: ~a" real-ms))
     (displayln (format "garbage collection mlliseconds mean: ~a" gc-ms))
     (displayln "")]))


(main)
