(ql:quickload :cl-ppcre)

(defun parse-pair (line)
  (multiple-value-bind (str array)
      (cl-ppcre:scan-to-strings "(\\d+)-(\\d+),(\\d+)-(\\d+)" line)
    (let ((parsed (map 'list #'parse-integer array)))
      (list `(,(first parsed) ,(second parsed))
            `(,(third parsed) ,(fourth parsed))))))

(defun contains (pair1 pair2)
  (and (<= (first pair1) (first pair2))
       (>= (second pair1) (second pair2))))

(defun redundant (pair1 pair2)
  (or (contains pair1 pair2)
      (contains pair2 pair1)))

(defun part-1 (file)
  (count-if #'(lambda (pair) (redundant (car pair) (cadr pair)))
                (mapcar #'parse-pair (uiop:read-file-lines file))))

(part-1 "input.txt")
 ; => 524 (10 bits, #x20C)

(defun overlap (pair1 pair2)
  (<= (max (first pair1) (first pair2))
      (min (second pair1) (second pair2))))

(defun part-2 (file)
  (count-if #'(lambda (pair) (overlap (car pair) (cadr pair)))
            (mapcar #'parse-pair (uiop:read-file-lines file))))

(part-2 "input.txt")
