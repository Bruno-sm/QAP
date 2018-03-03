#lang racket


(provide read-dat read-sln)


; .dat files
(define (read-dat file [flow-matrix-handler default-fmh] [distance-matrix-handler default-dmh])
  (define in-port (open-input-file file #:mode 'text))
  (define n-list (regexp-match #rx"[0-9]+" (read-line in-port)))
  (define n (string->number (first n-list)))
  (read-line in-port)
  (define fmh-result (flow-matrix-handler n in-port))
  (read-line in-port)
  (define dmh-result (distance-matrix-handler n in-port))
  (close-input-port in-port)
  (values n fmh-result dmh-result))

(define (default-fmh n in-port)
  (define flow-matrix (make-vector n (make-vector n 0)))
  (for ([i n])
    (define row (map (lambda (c) (string->number c)) (regexp-match* #rx"[0-9]+" (read-line in-port))))
    (vector-set! flow-matrix i (list->vector row)))
  flow-matrix)

(define (default-dmh n in-port)
  (define distance-matrix (make-vector n (make-vector n 0)))
  (for ([i n])
    (define row (map (lambda (c) (string->number c)) (regexp-match* #rx"[0-9]+" (read-line in-port))))
    (vector-set! distance-matrix i (list->vector row)))
  distance-matrix)

; .sln files
(define (read-sln file)
  (define in-port (open-input-file file #:mode 'text))
  (match-define
    (list size value)
    (map (lambda (c) (string->number c))
         (regexp-match* #rx"[0-9]+" (read-line in-port))))
  (define sol (map (lambda (c) (string->number c))
                   (regexp-match* #rx"[0-9]+" (port->string in-port #:close? #t))))
  (values size value sol))
