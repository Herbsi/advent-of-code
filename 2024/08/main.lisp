(ql:quickload :alexandria)
(ql:quickload :fset)
(ql:quickload :str)
(ql:quickload :iterate)

(use-package :iter)


(defstruct antenna
  (frequency #\. :type character)
  (x-pos 0 :type integer)
  (y-pos 0 :type integer))


(defun line->antennas (line y-pos)
  (iter
    (for x-pos upfrom 0)
    (for frequency in-string line)
    (unless (char= frequency #\.)
      (collect (make-antenna :frequency frequency :x-pos x-pos :y-pos y-pos)))))


(defun lines->antennas-table (lines)
  (iter
    (with antennas-table = (make-hash-table))
    (for y-pos upfrom 0)
    (for line in lines)
    (iter
      (for antenna in (line->antennas line y-pos))
      (setf (gethash (antenna-frequency antenna) antennas-table)
            (union (gethash (antenna-frequency antenna) antennas-table)
                   (list antenna)
                   :test #'equalp)))
    (finally (return antennas-table))))


(defun antinodes (antenna-pair &optional (steps '(-1 2)))
  (destructuring-bind (antenna-1 antenna-2) antenna-pair
    (let ((x-sign (signum (- (antenna-x-pos antenna-2) (antenna-x-pos antenna-1))))
          (y-sign (signum (- (antenna-y-pos antenna-2) (antenna-y-pos antenna-1))))
          (x-delta (abs (- (antenna-x-pos antenna-2) (antenna-x-pos antenna-1))))
          (y-delta (abs (- (antenna-y-pos antenna-2) (antenna-y-pos antenna-1)))))
      (iter
        (for i in steps)
        (collect
            (make-antenna :frequency #\#
                          :x-pos (+ (antenna-x-pos antenna-1) (* i x-sign x-delta))
                          :y-pos (+ (antenna-y-pos antenna-1) (* i y-sign y-delta))))))))


(defun main (file steps)
  (let* ((lines (uiop:read-file-lines file))
         (antennas-table (lines->antennas-table lines))
         (y-max (length lines))
         (x-max (length (car lines))))
    (flet ((valid-antinode? (antinode)
             (and (<= 0 (antenna-x-pos antinode) (1- x-max))
                  (<= 0 (antenna-y-pos antinode) (1- y-max)))))
      (iter
        (for (nil antennas) in-hashtable antennas-table)
        (for antennas-pairs = (remove-if (lambda (pair) (equalp (car pair) (cadr pair)))
                                         (alexandria:map-product #'list antennas antennas)))
        (unioning (remove-if-not #'valid-antinode? (mapcan
                                                    (lambda (pair) (antinodes pair steps))
                                                    antennas-pairs))
                  into antinodes test #'equalp)
        (finally
         (return (length (remove-duplicates antinodes :test #'equalp))))))))


(main "input.txt" '(-1 2)) ; => 409 (9 bits, #x199)
(main "input.txt" (alexandria:iota 101 :start -50)) ; => 1308 (11 bits, #x51C)
