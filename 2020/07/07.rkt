#lang racket/base

(require racket/match)

(require racket/list)

(define rev-edges? (make-parameter #f))

(define (add-edge-to-graph graph start end [amount 1])
  (hash-set (hash-set graph start (cons (list end amount)
                                        (hash-ref graph start null)))
            end (hash-ref graph end null)))

(define (get-edges-from-line line)
  ;; Returns list of lists
  ;; (list (list start end amount) ...)
  (let* ([colors (regexp-match* #px"((?:\\d+ )?\\w+ \\w+) bags?" line #:match-select cadr)]
         [end (string->symbol (car colors))]
         [colors (cdr colors)])
    (filter-map (lambda (no+color)
           (match no+color
             ["no other" #f]
             [(regexp #px"(\\d+) (\\w+ \\w+)"
                      (list _ (app string->number amount) (app string->symbol start)))
              ;; Part 1 & 2 require opposite edge direction
              `(,@(if (not (rev-edges?))
                      (list start end)
                      (list end start)) ,amount)])) colors)))

(define (add-line-to-graph graph line)
  (let iter ([graph graph]
             [edges (get-edges-from-line line)])
    (if (null? edges)
        graph
        (iter (apply add-edge-to-graph graph (car edges))
              (cdr edges)))))

(define (count-reachable graph start)
  ;; TODO make less stateful
  (define state (make-hasheq))
  (define pre (make-hasheq))
  (for ([key (hash-keys graph)])
    (hash-set! state key 'new)
    (hash-set! pre key #f))
  (define (depth vertex)
    (hash-set! state vertex 'visited)
    (for ([neighbor (map car (hash-ref graph vertex))])
      (when (eq? (hash-ref state neighbor) 'new)
        (hash-set! pre neighbor vertex)
        (depth neighbor)))
    (hash-set! state vertex 'saturated))
  (depth start)
  (- (length (filter (lambda (v)
                       (eq? v 'saturated))
                     (hash-values state)))
     1))                                ;; -1 to not count shiny-gold itself

(define (part-2 graph start)
  (let iter ([vertex start])
    (for/sum ([neighbor (map car (hash-ref graph vertex))]
              [weight (map cadr (hash-ref graph vertex))])
      ;; +1 for the neighbor bags themselves
      (* weight (+ 1 (iter neighbor))))))

;; Part 1
(call-with-input-file "input.txt"
  (lambda (in)
    ;; Part 1
    (define graph
      (let build-graph ([graph (make-immutable-hasheq)])
        (let ([line (read-line in)])
          (if (eof-object? line)
              graph
              (build-graph (add-line-to-graph graph line))))))
    (displayln (count-reachable graph '|shiny gold|))))

;; Part 2
(call-with-input-file "input.txt"
  (lambda (in)
    ;; Reverse edge direction
    (parameterize ([rev-edges? #t])         
      (define graph
        (let build-graph ([graph (make-immutable-hasheq)])
          (let ([line (read-line in)])
            (if (eof-object? line)
                graph
                (build-graph (add-line-to-graph graph line))))))
      (displayln (part-2 graph '|shiny gold|)))))

