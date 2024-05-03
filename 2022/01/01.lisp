(ql:quickload :str)

(defun parse-input (file)
  (mapcar #'(lambda (elf) (mapcar #'parse-integer (str:split-omit-nulls #\Newline elf)))
          (str:split "

" (uiop:read-file-string file))))

(defun sum (lst)
  (apply #'+ lst))

(defun elves-calories (elves)
  (mapcar #'sum elves))

(defun part-1 (file)
  (apply #'max (elves-calories (parse-input file))))

(part-1 "input.txt")
 ; => 74394 (17 bits, #x1229A)

(defun part-2 (file)
  (sum (subseq (sort (elves-calories (parse-input file)) #'>) 0 3)))

(part-2 "input.txt")
 ; => 212836 (18 bits, #x33F64)
