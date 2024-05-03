#lang racket/base

(require racket/vector)
(require racket/list)
(require racket/sequence)

(define (step board rows cols)
  (define (count-neighbors row col)
    (for*/sum ([i (in-list '(-1 0 1))]
               [j (in-list '(-1 0 1))])
      (define index (+ (* (+ row i) cols) col j))
      ;; don’t check cells where relative positions are too big or too small
      (if (or (= i j 0)                 ; don’t count current cell
              (< (+ col j) 0) (< (+ row i) 0)
              (>= (+ row i) rows) (>= (+ col j) cols))
          0
          (if (eq? 'occupied (vector-ref board index)) 1 0))))
  (for*/vector ([i (in-range rows)]
                [j (in-range cols)])
    (define elem (vector-ref board (+ (* i cols) j)))
    (cond [(eq? elem 'floor) 'floor]
          [(eq? elem 'empty) (if (= 0 (count-neighbors i j)) 'occupied 'empty)]
          [(eq? elem 'occupied) (if (>= (count-neighbors i j) 4) 'empty 'occupied)])))

(define (step-2 board rows cols)
  (define (at i j)
    (vector-ref board (+ (* i cols) j)))
  (define (count-visible row col)
    (define directions (make-hasheq
                        `(,(cons 'north `(,(in-range (sub1 row) -1 -1)
                                          ,(in-cycle `(,col))))
                          ,(cons 'north-east `(,(in-range (sub1 row) -1 -1)
                                               ,(in-range (add1 col) cols)))
                          ,(cons 'east `(,(in-cycle `(,row))
                                         ,(in-range (add1 col) cols)))
                          ,(cons 'south-east `(,(in-range (add1 row) rows)
                                               ,(in-range (add1 col) cols)))
                          ,(cons 'south `(,(in-range (add1 row) rows)
                                          ,(in-cycle `(,col))))
                          ,(cons 'south-west `(,(in-range (add1 row) rows)
                                               ,(in-range (sub1 col) -1 -1)))
                          ,(cons 'west `(,(in-cycle `(,row))
                                         ,(in-range (sub1 col) -1 -1)))
                          ,(cons 'north-west `(,(in-range (sub1 row) -1 -1)
                                               ,(in-range (sub1 col) -1 -1))))))
    (count values (map (lambda (indices)
                         ;; returns true if an occupied seat is visible
                         (for/first ([i (car indices)]
                                     [j (cadr indices)]
                                     #:unless (eq? 'floor (at i j)))
                           (eq? (at i j) 'occupied)))
                       (hash-values directions))))
  (for*/vector ([i (in-range rows)]
                [j (in-range cols)])
    (cond [(eq? (at i j) 'floor) 'floor]
          [(eq? (at i j) 'empty) (if (= 0 (count-visible i j)) 'occupied 'empty)]
          [(eq? (at i j) 'occupied) (if (>= (count-visible i j) 5) 'empty 'occupied)])))

(call-with-input-file "input.txt"
  (lambda (in)
    (let* ([rows (map (lambda (line)
                        (for/vector ([char line])
                          (cond [(char=? #\. char) 'floor]
                                [(char=? #\L char) 'empty]
                                [(char=? #\# char) 'occupied])))
                      (sequence->list (in-lines in)))]
           [cols (vector-length (car rows))]
           [board (apply vector-append rows)]
           [rows (/ (vector-length board) cols)])
      (println (let iter ([prev #()]
                          [curr board])
                 (if (equal? prev curr)
                     (vector-count (lambda (x) (eq? x 'occupied)) curr)
                     (iter curr (step curr rows cols)))))
      (let iter ([prev #()]
                 [curr board])
        (if (equal? prev curr)
            (vector-count (lambda (x) (eq? x 'occupied)) curr)
            (iter curr (step-2 curr rows cols)))))))
