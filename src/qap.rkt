#lang racket

(require racket/cmdline)
(require "data-reader.rkt" "qap-representation-utils.rkt")
(require "greedy.rkt" "local-search.rkt" "tabu-search.rkt" "genetic.rkt" "genetic-operators.rkt")


(struct cli-options (algorithm
                     executions-number
                     max-evaluations
                     seed
                     output-mode)
                    #:mutable)

(define (main)
  (define options (cli-options 'greedy 1 50000 (current-seconds) 'human-readable))
  (define files empty)
  (command-line
    #:program "qap"
    #:once-each
    [("-m" "--max-evaluations")
     me
     "Maximum number of evaluations inside an algorithm"
     (set-cli-options-max-evaluations! options (string->number me))]
    [("-r" "--repetitions")
     exec-number
     "Number of executions for each case. The result is the mean of all executions"
     (set-cli-options-executions-number! options (string->number exec-number))]
    [("-s" "--seed")
     seed 
     "seed for the random numbers generator"
     (set-cli-options-seed! options (string->number seed))]
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
    [("--genetic-gen-pos")
     "Uses a generational genetic algorithm with positional crossover"
     (set-cli-options-algorithm! options 'gen-gp)]
    [("--genetic-gen-pmx")
     "Uses a generational genetic algorithm with PMX crossover"
     (set-cli-options-algorithm! options 'gen-gpmx)]
    [("--genetic-st-pos")
     "Uses a steady genetic algorithm with positional crossover"
     (set-cli-options-algorithm! options 'gen-sp)]
    [("--genetic-st-pmx")
     "Uses a steady genetic algorithm with PMX crossover"
     (set-cli-options-algorithm! options 'gen-spmx)]
    [("--memetic-all-pos")
     "Uses a generational genetic algorithm with positional crossover and local search over all the population every 10 generations"
     (set-cli-options-algorithm! options 'mem-all-pos)]
    [("--memetic-rand-pos")
     "Uses a generational genetic algorithm with positional crossover and local search over a random 10% of the population every 10 generations"
     (set-cli-options-algorithm! options 'mem-rand-pos)]
    [("--memetic-best-pos")
     "Uses a generational genetic algorithm with positional crossover and local search over the best 10% of the population every 10 generations"
     (set-cli-options-algorithm! options 'mem-best-pos)]
    [("--memetic-all-pmx")
     "Uses a generational genetic algorithm with PMX crossover and local search over all the population every 10 generations"
     (set-cli-options-algorithm! options 'mem-all-pmx)]
    [("--memetic-rand-pmx")
     "Uses a generational genetic algorithm with PMX crossover and local search over a random 10% of the population every 10 generations"
     (set-cli-options-algorithm! options 'mem-rand-pmx)]
    [("--memetic-best-pmx")
     "Uses a generational genetic algorithm with PMX crossover and local search over the best 10% of the population every 10 generations"
     (set-cli-options-algorithm! options 'mem-best-pmx)]
    #:args (file . more-files) (set! files (cons file more-files)))
  (random-seed (cli-options-seed options))
  (set! files (map (lambda (f) (build-path (current-directory) f)) files))  
  (cond [(symbol=? (cli-options-output-mode options) 'csv)
         (displayln "Case,size,repetitions,best_value,values_mean,values_deviation,opt_value,cpu_ms,real_ms,gc_ms")])
  (match-define-values (algorithm alg-args)
    (let ([alg (cli-options-algorithm options)])
      (cond
        [(symbol=? alg 'greedy) (values greedy empty)]
        [(symbol=? alg 'local-bf)
         (values local-search `(,(cli-options-max-evaluations options)
                                ,best-first-dlb-selection))]
        [(symbol=? alg 'local-bn)
         (values local-search `(,(cli-options-max-evaluations options)
                                ,best-neighbor-selection))]
        [(symbol=? alg 'local-vnd)
         (values local-search `(,(cli-options-max-evaluations options)
                                ,vnd-selection))]
        [(symbol=? alg 'stm-tabu)
         (values stm-tabu-search `(,(cli-options-max-evaluations options)))]
        [(symbol=? alg 'gen-gp)
         (values genetic-generational `(,(cli-options-max-evaluations options)
                                        ,position-crossover
                                        50 0.7 0.001))]
        [(symbol=? alg 'gen-gpmx)
         (values genetic-generational `(,(cli-options-max-evaluations options)
                                        ,pmx-crossover
                                        50 0.7 0.001))]
        [(symbol=? alg 'gen-sp)
         (values genetic-steady `(,(cli-options-max-evaluations options)
                                  ,position-crossover
                                  50 0.001))]
        [(symbol=? alg 'gen-spmx)
         (values genetic-steady `(,(cli-options-max-evaluations options)
                                  ,pmx-crossover
                                  50 0.001))]
        [(symbol=? alg 'mem-all-pos)
         (values memetic-all `(,(cli-options-max-evaluations options)
                               ,position-crossover
                               10 0.7 0.001 10))]
        [(symbol=? alg 'mem-rand-pos)
         (values memetic-rand `(,(cli-options-max-evaluations options)
                               ,position-crossover
                               10 0.7 0.001 10))]
        [(symbol=? alg 'mem-best-pos)
         (values memetic-best `(,(cli-options-max-evaluations options)
                               ,position-crossover
                               10 0.7 0.001 10))]
        [(symbol=? alg 'mem-all-pmx)
         (values memetic-all `(,(cli-options-max-evaluations options)
                               ,pmx-crossover
                               10 0.7 0.001 10))]
        [(symbol=? alg 'mem-rand-pmx)
         (values memetic-rand `(,(cli-options-max-evaluations options)
                               ,pmx-crossover
                               10 0.7 0.001 10))]
        [(symbol=? alg 'mem-best-pmx)
         (values memetic-best `(,(cli-options-max-evaluations options)
                               ,pmx-crossover
                               10 0.7 0.001 10))])))

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
