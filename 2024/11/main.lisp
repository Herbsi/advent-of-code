(ql:quickload :alexandria)
(ql:quickload :fset)
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
    ((list 0) (list (list 1 stone)))
    ((trivia:guard digits
                   (evenp (length digits)))
     (list (list (digits-to-number (subseq digits 0 (/ (length digits) 2))) stone)
           (list (digits-to-number (subseq digits (/ (length digits) 2))) stone)))
    (_ (list (list (* stone 2024) stone)))))


(defun blink (stones)
  (mapcan #'change-stone stones))


(defun part-1 (filename blinks)
  (iter
    (for blink from 0 below blinks)
    (for stones initially (parse-stones filename) then (blink stones))
    (finally (return (length stones)))))


(part-1 "input.txt" 25) ; => 220999 (18 bits, #x35F47)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun part-2 (filename blinks)
  (iter
    (for blink from 0 below blinks)

    (for counts next (make-hash-table))
    (for old-counts previous counts initially (make-hash-table))

    (if-first-time
     (iter
       (for stone in (parse-stones filename))
       (incf (gethash stone old-counts 0))))

    (for stones next (alexandria:hash-table-keys old-counts))
    (iter
      (for (stone origin) in (blink stones))
      (incf (gethash stone counts 0)
            (gethash origin old-counts)))

    (finally (return (iter
                       (for (stone count) in-hashtable counts)
                       (summing count))))))


(part-2 "input.txt" 75) ; => 261936432123724 (48 bits, #xEE3AD3FD8F4C)
