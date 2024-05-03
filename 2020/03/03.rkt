#! /usr/bin/env racket

#lang racket/base

(require racket/list)
(require racket/sequence)
(require racket/stream)

(define (process-lines lines (slope (list 3 1)))
  (let ([x (first slope)]
        [y (second slope)]
        [w (string-length (first lines))])
    (for/fold ([sum 0])
              ([line (rest lines)]
               [n (stream-rest (in-naturals))] ;; skip 0
               #:when (= 0 (modulo n y)))
      (if (char=? #\# (string-ref line
                                  (modulo (* x (floor (/ n y))) w)))
          (+ sum 1)
          sum))))

;; Part 1
(call-with-input-file "input.txt"
  (lambda (in) (process-lines (sequence->list (in-lines in)))))

;; Part 2
(call-with-input-file "input.txt"
  (lambda (in)
    (let ([lines (sequence->list (in-lines in))])
      (* (process-lines lines (list 1 1))
         (process-lines lines)
         (process-lines lines (list 5 1))
         (process-lines lines (list 7 1))
         (process-lines lines (list 1 2))))))


