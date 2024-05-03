#lang racket/base

(struct ship (x y direction)
  #:transparent)

(struct waypoint (x y)
  #:transparent)

(define (manhatten boat)
  (+ (abs (ship-x boat))
     (abs (ship-y boat))))

(define compass
  #hasheq((0 . E)
          (90 . N)
          (180 . W)
          (270 . S)))

(define (move-boat boat dir value)
  (cond
    [(eq? dir 'F) (move-boat boat (hash-ref compass (ship-direction boat)) value)]
    [(eq? dir 'N) (ship (ship-x boat) (+ (ship-y boat) value) (ship-direction boat))]
    [(eq? dir 'S) (move-boat boat 'N (- value))]
    [(eq? dir 'E) (ship (+ (ship-x boat) value) (ship-y boat) (ship-direction boat))]
    [(eq? dir 'W) (move-boat boat 'E (- value))]
    [(eq? dir 'L) (ship (ship-x boat) (ship-y boat)
                        (modulo (+ (ship-direction boat) value)
                                360))]
    [(eq? dir 'R) (move-boat boat 'L (modulo (- value) 360))]))

(define (move-waypoint wp boat dir value)
  (cond
    [(eq? dir 'F) (values wp (move-boat (move-boat boat 'E (* value (waypoint-x wp)))
                                        'N (* value (waypoint-y wp))))]                   
    [(eq? dir 'N) (values (waypoint (waypoint-x wp) (+ (waypoint-y wp) value))
                          boat)]
    [(eq? dir 'S) (move-waypoint wp boat 'N (- value))]
    [(eq? dir 'E) (values (waypoint (+ (waypoint-x wp) value) (waypoint-y wp))
                          boat)]
    [(eq? dir 'W) (move-waypoint wp boat 'E (- value))]
    [(eq? dir 'L) (if (zero? value)
                      (values wp boat)
                      (move-waypoint (waypoint (- (waypoint-y wp))
                                               (waypoint-x wp))
                                     boat 'L (- value 90)))]
    [(eq? dir 'R) (move-waypoint wp boat 'L (modulo (- value) 360))]))

(call-with-input-file "input.txt"
  (lambda (in)
    (define instructions
      (for/list ([line (in-lines in)])
        (define match (regexp-match #px"([FNSEWRL])(\\d+)" line))
        (list (string->symbol (cadr match))
              (string->number (caddr match)))))
    (displayln (manhatten
                (for/fold ([boat (ship 0 0 0)])
                          ([instr (in-list instructions)])
                  (apply move-boat boat instr))))
    (displayln
     (let-values ([(wp boat)
                   (for/fold ([wp (waypoint 10 1)]
                              [boat (ship 0 0 0)])
                             ([instr (in-list instructions)])
                     (apply move-waypoint wp boat instr))])
       (manhatten boat)))))

