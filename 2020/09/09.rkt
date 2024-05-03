#lang racket/base

(require racket/list)

(define (sum-of-two lst n)
  ;; TODO this doesn’t work yet
  (for*/or ([i (in-list lst)]
            [j (in-list lst)])
    (and (= n (+ i j)))))

(define (part-1 input [preamble-length 25])
  (for/first ([n (in-naturals)]
              [current (in-list (drop input preamble-length))]
              #:when (not (sum-of-two (take (drop input n)
                                            preamble-length)
                                      current)))
    current))

(define (part-2 input value)
  (let iter ([remaining input]
             [current-value 0]
             [current-seq null])
    (cond [(= current-value value)
           (+ (apply min current-seq)
              (apply max current-seq))]
          [(> current-value value)
           (iter remaining
                 (- current-value (car current-seq))
                 (cdr current-seq))]
          [(< current-value value)
           (iter (cdr remaining)
                 (+ current-value (car remaining))
                 (append current-seq (list (car remaining))))])))

(call-with-input-file "input.txt"
  (lambda (in)
    (let* ([input (for/list ([line (in-lines in)]) (string->number line))]
           [sol-1 (part-1 input)])
      (displayln sol-1)
      (displayln (part-2 input sol-1)))))
