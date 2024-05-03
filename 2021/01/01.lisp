;; We can make use that in an array a + b + c < b + c + d <=> a < d
;; This version allows us to write a generic version like so

(defun count-increments (lst offset)
  (count-if (lambda (n) (> n 0))
            (mapcar #'- (nthcdr offset lst) lst)))

(defun read-numbers (file &optional (type 'list))
  (map type #'parse-integer (uiop:read-file-lines file)))

(defun part-1-better (file)
  (count-increments (read-numbers file) 1))

(defun part-2-better (file)
  (count-increments (read-numbers file) 3))

(part-1-better "input")
 ; => 1711 (11 bits, #x6AF)
(part-2-better "input")
 ; => 1743 (11 bits, #x6CF)
