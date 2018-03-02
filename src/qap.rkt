#lang racket

(require racket/cmdline)
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
    [("-g" "--greedy") "Uses the greedy algorithm" 
           (set! mode 'greedy)]
    [("-l" "--local-search") "Uses the local search algorithm"
           (set! mode 'local)]
    #:args (file . more-files) (set! files (cons file more-files)))
  (set! files (map (lambda (f) (build-path (current-directory) f)) files))
  
  (cond
    [(symbol=? mode 'greedy)
     (for-each (lambda (file) (greedy file max-iterations)) files)]
    [(symbol=? mode 'local)
     (for-each (lambda (file) (local-search file max-iterations)) files)]))


(main)
