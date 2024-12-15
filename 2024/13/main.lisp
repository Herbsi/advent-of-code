(ql:quickload :cl-ppcre)
(ql:quickload :iterate)

(use-package :iter)


(let ((ptrn (ppcre:create-scanner "Button ([AB]): X\\+(\\d+), Y\\+(\\d+)")))
  (defun parse-button (string)
    (ppcre:register-groups-bind
        (button (#'parse-integer x y))
        (ptrn string)
      (list button x y))))


(let ((ptrn (ppcre:create-scanner "Prize: X=(\\d+), Y=(\\d+)")))
  (defun parse-prize (string)
    (ppcre:register-groups-bind
        ((#'parse-integer x y))
        (ptrn string)
      (list 'prize x y))))


(defun parse-machine (machine)
  (list (parse-button (car machine))
        (parse-button (cadr machine))
        (parse-prize (caddr machine))))


(defun parse-input (filename)
  (iter
    (for machine on (uiop:read-file-lines filename) by #'cddddr)
    (collect (parse-machine machine))))


(defun solve-machine (machine)
  (iter outer
    (with button-a = (car machine))
    (with button-b = (cadr machine))
    (with prize = (caddr machine))
    (for a from 1 to 100)
    (iter
      (for b from 1 to 100)
      (when (and (= (cadr prize) (+ (* (cadr button-a) a) (* (cadr button-b) b)))
                 (= (caddr prize) (+ (* (caddr button-a) a) (* (caddr button-b) b))))
        (in outer (minimizing (+ (* a 3) (* b 1))))))))


(defun part-1 (filename)
  (iter
    (for machine in (parse-input filename))
    (for tokens = (solve-machine machine))
    (when tokens
      (summing tokens))))


(part-1 "input.txt") ; => 32026 (15 bits, #x7D1A)


(let ((ptrn (ppcre:create-scanner "Prize: X=(\\d+), Y=(\\d+)")))
  (defun parse-prize-2 (string)
    (ppcre:register-groups-bind
        ((#'parse-integer x y))
        (ptrn string)
      (list 'prize (+ x 10000000000000) (+ y 10000000000000)))))


(defun parse-machine-2 (machine)
  (list (parse-button (car machine))
        (parse-button (cadr machine))
        (parse-prize-2 (caddr machine))))


(defun parse-input-2 (filename)
  (iter
    (for machine on (uiop:read-file-lines filename) by #'cddddr)
    (collect (parse-machine-2 machine))))


(defun solve-machine-2 (machine)
  (let* ((button-a (car machine))
         (button-b (cadr machine))
         (prize (caddr machine))
         (det (- (* (cadr button-a) (caddr button-b))
                 (* (cadr button-b) (caddr button-a))))
         (a-pushes (and (not (zerop det))
                        (/ (- (* (caddr button-b) (cadr prize))
                              (* (cadr button-b) (caddr prize)))
                           det)))
         (b-pushes (and (not (zerop det))
                        (/ (- (* (cadr button-a) (caddr prize))
                              (* (caddr button-a) (cadr prize)))
                           det))))
    (and (not (zerop det))
         (integerp a-pushes) (integerp b-pushes)
         (<= 0 a-pushes) (<= 0 b-pushes)
         (+ (* 3 a-pushes) (* 1 b-pushes)))))


(defun part-2 (filename)
  (iter
    (for machine in (parse-input-2 filename))
    (for tokens = (solve-machine-2 machine))
    (when tokens
      (summing tokens))))


(part-2 "input.txt") ; => 89013607072065 (47 bits, #x50F5186E0141)
