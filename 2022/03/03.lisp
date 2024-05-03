(ql:quickload :cl-herbsi)
(ql:quickload :str)

(defun str->list (str)
  (map 'list #'(lambda (c) c) str))

(defun parse-line-1 (line)
  (let* ((l (length line))
         (compartment-1 (str->list (str:substring 0 (/ l 2) line)))
         (compartment-2 (str->list (str:substring (/ l 2) t line))))
    (car (intersection compartment-1 compartment-2))))

(defun priority (char)
  (if (char<= #\a char #\z)
      (+ (- (char-int char) (char-int #\a)) 1)
      (+ (- (char-int char) (char-int #\A)) 27)))

(defun part-1 (file)
  (apply #'+ (mapcar (lambda (line) (priority (parse-line-1 line)))
                     (uiop:read-file-lines file))))

(part-1 "input.txt")
 ; => 8088 (13 bits, #x1F98)

(defun group (lines)
  (priority (car (reduce #'intersection lines))))

(defun part-2 (file)
  (apply #'+ (mapcar #'group
                     (cl-herbsi:group
                      (mapcar #'str->list (uiop:read-file-lines file)) 3))))

(part-2 "input.txt")
 ; => 2522 (12 bits, #x9DA)
