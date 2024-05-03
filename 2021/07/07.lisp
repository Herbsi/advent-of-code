(ql:quickload :alexandria)
(ql:quickload :str)

(defun read-positions (file)
  (mapcar #'parse-integer (str:split "," (uiop:read-file-string file))))

(defun part-1 (file)
  (let* ((positions (read-positions file))
         (alignment-position (alexandria:median positions)))
    (flet ((fuel (target)
             (apply #'+ (mapcar (lambda (x) (abs (- x alignment-position))) positions))))
      (fuel alignment-position))))

(defun sum-1-to-n (n)
  (/ (* n (1+ n)) 2))

(defun part-2 (file)
  (let* ((positions (read-positions file))
         (alignment-position (alexandria:mean positions)))
    (flet ((fuel (target)
             (apply #'+ (mapcar (lambda (x) (sum-1-to-n (abs (- target x)))) positions))))
      ;; Because the target position needs to be an integer, we calculate the fuel needed to reach ⌊target⌋ and ⌈target⌉ and take the lower value
      (min (fuel (floor alignment-position)) (fuel (ceiling alignment-position))))))

(part-1 "input")
 ; => 333755 (19 bits, #x517BB)
(part-2 "input")
 ; => 94017638 (27 bits, #x59A9866)
