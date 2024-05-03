#lang racket/base

(require racket/file)
(require racket/string)

(define key
  #hash((#\F . 0)
        (#\B . 1)
        (#\L . 0)
        (#\R . 1)))

(define (get-row str)
  (for/fold ([sum 0])
            ([n '(6 5 4 3 2 1 0)]
             [char str])
    (+ sum (* (hash-ref key char) (expt 2 n)))))

(define (get-col str)
  (for/fold ([sum 0])
            ([n '(2 1 0)]
             [char str])
    (+ sum (* (hash-ref key char) (expt 2 n)))))

(define (calc-id row col)
  (+ (* row 8) col))

(define (seat-id seat)
  (calc-id (get-row (substring seat 0 7))
           (get-col (substring seat 7))))

;; Part 1
(apply max (map seat-id (string-split (file->string "input.txt") "\n")))

;; Part 2
(let ([ids (sort (map seat-id (string-split (file->string "input.txt") "\n")) <)])
  (for ([x (in-list ids)]
        [y (in-list (cdr ids))]
        #:when (and (> x 7) (= y (+ x 2)))
        #:final (and (> x 7) (= y (+ x 2))))
    (displayln (+ 1 x))))
