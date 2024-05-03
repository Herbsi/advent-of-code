(ql:quickload :str)
(ql:quickload :trivia)
(use-package :trivia)

(defun read-population (file)
  ;; iter would not, for the life of me, work here for some reason
  (let ((population (mapcar #'parse-integer (str:split-omit-nulls "," (uiop:read-file-string file)))))
    (mapcar (lambda (n) (count n population)) `(0 1 2 3 4 5 6 7 8))))

(defun advance-population (population)
  ;; TODO More elegant ?
  (match population
    ((list zero one two three four five six seven eight)
     (list one two three four five six (+ seven zero) eight zero))))

(defun part-1 (file &optional (days 80))
  (labels ((rec (population n)
             (if (= n days)
                 (apply #'+ population)
                 (rec (advance-population population) (1+ n)))))
    (rec (read-population file) 0)))

 (part-1 "input")
 ; => 387413 (19 bits, #x5E955)

(defun part-2 (file)
  (part-1 file 256))

(part-2 "input")
 ; => 1738377086345 (41 bits, #x194BF594589)
