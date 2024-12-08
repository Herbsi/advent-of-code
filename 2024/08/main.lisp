(ql:quickload :alexandria)
(ql:quickload :fset)
(ql:quickload :str)
(ql:quickload :iterate)

(use-package :iter)

(defun frequency (antenna)
  (car antenna))


(defun pos-x (antenna)
  (cadr antenna))


(defun pos-y (antenna)
  (caddr antenna))


(defun make-antenna (frequency pos-x pos-y)
  (list frequency pos-x pos-y))


(defun antinodes (antennas)
  (flet ((antinodes-from-pair (antenna-1 antenna-2)
           (let* ((sign-x (signum (- (pos-x antenna-2) (pos-x antenna-1))))
                  (sign-y (signum (- (pos-y antenna-2) (pos-y antenna-1))))
                  (delta-x (abs (- (pos-x antenna-2) (pos-x antenna-1))))
                  (delta-y (abs (- (pos-y antenna-2) (pos-y antenna-1)))))
             (list
              (list
               (+ (pos-x antenna-1) (* sign-x 2 delta-x))
               (+ (pos-y antenna-1) (* sign-y 2 delta-y)))
              (list
               (- (pos-x antenna-2) (* sign-x 2 delta-x))
               (- (pos-y antenna-2) (* sign-y 2 delta-y)))))))

    (reduce #'union (remove-if (lambda (lst) (equal (car lst) (cadr lst)))
                               (alexandria:map-product #'antinodes-from-pair antennas antennas)))))


(defun file-line->antennas (line pos-y)
  (iter
    (for pos-x upfrom 0)
    (for frequency in-string line)
    (unless (char= frequency #\.)
      (collect (make-antenna frequency pos-x pos-y)))))


(defun lines->antennas-table (lines)
  (iter
    (with antennas-table = (make-hash-table))
    (for pos-y upfrom 0)
    (for line in lines)
    (iter
      (for antenna in (file-line->antennas line pos-y))
      (setf (gethash (frequency antenna) antennas-table)
            (union (gethash (frequency antenna) antennas-table)
                   (list antenna)
                   :test #'equal)))
    (finally (return antennas-table))))


(defun valid-antinode? (antinode max-x max-y)
  (and (<= 0 (car antinode) (1- max-x))
       (<= 0 (cadr antinode) (1- max-y))))


(let* ((lines (uiop:read-file-lines "input.txt"))
       (antennas-table (lines->antennas-table lines))
       (max-y (length lines))
       (max-x (length (car lines))))
  (iter
    (for (frequency antennas) in-hashtable antennas-table)
    (unioning (antinodes antennas) into antinodes test #'equal)

    (finally
     (return (length (remove-duplicates (remove-if-not


                                         (lambda (antinode) (valid-antinode? antinode max-x max-y))
                                         antinodes)
                                        :test #'equal))))))
                                        ; => 409 (9 bits, #x199)
