(ql:quickload :iterate)
(ql:quickload :str)
(ql:quickload :trivia)

(use-package :iter)

(defun parse-stones (filename)
  (mapcar #'parse-integer (str:split #\Space (uiop:read-file-line filename))))


(defun number-to-digits (number)
  (labels ((rec (number digits)
             (if (zerop number)
                 digits
                 (multiple-value-bind (number digit) (floor number 10)
                   (rec number (cons digit digits))))))
    (if (zerop number)
        '(0)
        (rec number nil))))


(defun digits-to-number (digits)
  (iter
    (for power upfrom 0)
    (for digit in (reverse digits))
    (summing (* digit (expt 10 power)))))


(defun change-stone (stone)
  (trivia:match (number-to-digits stone)
    ((list 0) (list 1))
    ((trivia:guard digits
                   (evenp (length digits)))
     (list (digits-to-number (subseq digits 0 (/ (length digits) 2)))
           (digits-to-number (subseq digits (/ (length digits) 2)))))
    (_ (list (* stone 2024)))))


(defun blink (stones)
  (mapcan #'change-stone stones))


(defun part-1 (filename)
  (iter
    (for blinks from 0 below 25)
    (for stones initially (parse-stones filename) then (blink stones))
    (finally (return (length stones)))))


(part-1 "input.txt") ; => 220999 (18 bits, #x35F47)
