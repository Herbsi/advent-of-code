(ql:quickload :cl-herbsi)
(ql:quickload :str)
(ql:quickload :iterate)
(use-package :iter)
(ql:quickload :fset)
(use-package :fset-user)

(defun from-digits (digits &key (radix 10))
  (iter
    (for i upfrom 0)
    (for d in (reverse digits))
    (summing (* d (expt radix i)))))

(from-digits '(1 0 0) :radix 2)

(defun characters (str)
  (iter
    (for char in-string str)
    (collect (parse-integer (string char)))))

(defun read-lines-into-list (file)
  (mapcar #'characters (uiop:read-file-lines file)))

(defun gamma-rate (lst)
  (iter
    (with n = (length lst))
    (for line in lst)
    (reducing line by (lambda (curr acc) (mapcar #'+ curr acc)) into gamma-rate)
    (finally (return (let* ((gamma-rate (mapcar (lambda (value) (if (> value (/ n 2)) 1 0)) gamma-rate))
                            (epsilon-rate (mapcar (lambda (value) (mod (1- value) 2)) gamma-rate)))
                       (values (from-digits gamma-rate :radix 2) (from-digits epsilon-rate :radix 2)))))))

(defun part-1 (file)
  (multiple-value-bind (gamma epsilon) (gamma-rate (read-lines-into-list file))
    (* gamma epsilon)))

(part-1 "input")
 ; => 4001724 (22 bits, #x3D0FBC)

                                        ; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rating (bag cmp)
  (labels ((rec (bag pos)
             (if (= (fset:set-size bag) 1)
                 (from-digits (fset:arb bag) :radix 2)
                 (let* ((ones (fset:filter (lambda (lst) (= 1 (nth pos lst))) bag))
                        (zeros (fset:bag-difference bag ones)))
                   (if (funcall cmp (fset:size ones) (fset:size zeros))
                       (rec ones (1+ pos))
                       (rec zeros (1+ pos)))))))
    (rec bag 0)))

(defun oxygen-rating (bag)
  (rating bag #'>=))

(defun co2-rating (bag)
  (rating bag #'<))

(defun part-2 (file)
  (let ((numbers (fset:convert 'fset:bag (read-lines-into-list file))))
    (* (oxygen-rating numbers) (co2-rating numbers))))

(part-2 "input")
 ; => 587895 (20 bits, #x8F877)
