#lang racket/base

(require racket/file)
(require racket/list)
(require racket/string)

;; Part 1
(define (part-1 str)
  (apply + (map (lambda (group)
                  (count (lambda (char-list) (not (char=? (car char-list) #\newline)))
                         (group-by (lambda (x) x) (string->list group))))
                (string-split str "\n\n"))))

(define (part-2 str)
  (apply + (map (lambda (group)
                  (let* ([answers (string->list group)]
                         [people (+ 1 (count (lambda (c) (char=? c #\newline))
                                             answers))])
                    (count (lambda (no-of-answers) (= no-of-answers people))
                           (map length (group-by (lambda (x) x) answers)))))
                (string-split str "\n\n"))))

(let ([input (file->string "input.txt")])
  (displayln (part-1 input))
  (displayln (part-2 input)))
