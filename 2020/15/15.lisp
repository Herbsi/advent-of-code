(ql:quickload :alexandria)

(ql:quickload :iterate)
(use-package :iter)

(defconstant input '(1 0 15 2 10 13))

(defun part-1 (numbers &optional (turns 2020))
  (let ((age (alexandria:alist-hash-table
              (iter (for turn upfrom 1)
                (for number in numbers)
                (collect (cons number turn)))))
        (spoken-count (alexandria:alist-hash-table
                       (mapcar (lambda (number) (cons number 1)) numbers))))
    (iter
      (for prev previous number initially (nth (1- (length numbers)) numbers))
      (for turn upfrom (1+ (length numbers)))
      (for number =
           (if (= 1 (gethash prev spoken-count))
               0
               ;; prev was most recently spoken just before this turn
               (- turn 1 (gethash prev age))))
      ;; (format t "turn: ~a no: ~a~%" turn number)
      ;; Update the age of prev later
      (setf (gethash prev age) (1- turn))
      (setf (gethash number spoken-count)
            (1+ (or (gethash number spoken-count) 0)))
      (while (< turn turns))
      (finally (return number)))))

(format t "~a~%" (part-1 input))
(format t "~a~%" (part-1 input 30000000))
