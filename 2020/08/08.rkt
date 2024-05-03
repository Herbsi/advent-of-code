#lang racket/base

(require racket/string)

(define (read-instructions file-port)
  (call-with-input-file file-port
    (lambda (in)
      (for/vector ([line (in-lines in)])
        (let* ([instr-no (string-split line)]
               [instr (string->symbol (car instr-no))]
               [no (string->number (cadr instr-no))])
          (list instr no))))))

(define (execute instruction-vector)
  (define executed? (make-vector (vector-length instruction-vector) #f))
  (let iter ([acc 0]
             [pos 0])
    (cond [(= pos (vector-length instruction-vector)) (list 'done acc)]
          [(vector-ref executed? pos) (list 'loop acc)]
          [else
           (let ([instruction (car (vector-ref instruction-vector pos))]
                 [no (cadr (vector-ref instruction-vector pos))])
             (vector-set! executed? pos #t)
             (case instruction
               ['acc (iter (+ acc no)
                           (+ pos 1))]
               ['jmp (iter acc
                           (+ pos no))]
               ['nop (iter acc
                           (+ pos 1))]))])))

;; Part 1
(let ([instructions (read-instructions "input.txt")])
  ;; Part 1
  (displayln (cadr (execute instructions)))
  ;; Part 2
  (define (swap-instruction! pos)
    (let ([instr-no (vector-ref instructions pos)])
      (case (car instr-no)
        ['acc #f]
        ['jmp (vector-set! instructions pos (list 'nop (cadr instr-no)))]
        ['nop (vector-set! instructions pos (list 'jmp (cadr instr-no)))])))
  (for ([pos (in-range (vector-length instructions))])
    (when (swap-instruction! pos)
      (let ([result (execute instructions)])
        (when (eq? 'done (car result))
          (displayln (cadr result)))
        (swap-instruction! pos)))))
