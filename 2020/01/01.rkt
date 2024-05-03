#! /usr/bin/env racket

#lang racket/base

(require data/collection)

(define (solve numbers)
  (for ([lst (in-sequences (cartesian-product numbers numbers))])
    (let ([i (first lst)]
          [j (second lst)])
      (when (= 2020 (+ i j))
        (displayln (* i j))))))

(define (solve3 numbers)
  (for ([lst (in-sequences (cartesian-product numbers numbers numbers))])
    (let ([i (first lst)]
          [j (second lst)]
          [k (third lst)])
      (when (= 2020 (+ i j k))
        (displayln (* i j k))))))

(call-with-input-file "input.txt"
  (lambda (in)
    (let ([numbers (for/list ([line (in-lines in)])
                     (string->number line))])
      (solve numbers)
      (solve3 numbers))))

