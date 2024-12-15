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
