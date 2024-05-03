#! /usr/bin/env racket

#lang racket/base

(require racket/list)
(require racket/match)
(require racket/sequence)

(define line-rx #px"(\\d+)-(\\d+) ([a-z]): ([a-z]+)")

(define (valid-line? line)
  (match-let ([(list _ lower upper char password) (regexp-match line-rx line)])
    (let ([lower (string->number lower)]
          [upper (string->number upper)]
          [char (string-ref char 0)])
      (<= lower (count (lambda (c) (char=? c char))
                       (string->list password))
          upper))))

(define (valid-line?2 line)
  
  (match-let ([(list _ lower upper char password) (regexp-match line-rx line)])
    (let* ([first (string->number lower)]
           [second (string->number upper)]
           [char (string-ref char 0)]
           [ischar (lambda (c) (char=? c char))])
      (match (list (string-ref password (- first 1))
                   (string-ref password (- second 1)))
        [(list (? ischar) (? ischar)) #f]
        [(list (? ischar) _) #t]
        [(list _ (? ischar)) #t]
        [_ #f]))))

(call-with-input-file "input.txt"
  (lambda (in)
    (let ([lines (sequence->list (in-lines in))])
      (values (count valid-line? lines)
              (count valid-line?2 lines)))))
