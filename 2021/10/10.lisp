(ql:quickload :iterate)
(use-package :iter)
(ql:quickload :trivia)
(use-package :trivia)
(ql:quickload :alexandria)

(defun parse-line (line)
  (iter
    (with stack = nil)
    (for c in-string line)
    (if (member c '(#\( #\[ #\{ #\<))
        (push c stack)
        (match `(,(pop stack) ,c)
          ((or (list #\( #\)) (list #\[ #\]) (list #\{ #\}) (list #\< #\>)) (next-iteration))
          (_ (leave c))))
    (finally (return (if (null stack) 'ok stack)))))

(let ((score (alexandria:alist-hash-table `((#\) . 3) (#\] . 57) (#\} . 1197) (#\> . 25137)))))
  (defun checker-score (char)
    (gethash char score 0)))

(defun part-1 (file)
  (apply #'+ (mapcar (lambda (line) (checker-score (parse-line line))) (uiop:read-file-lines file))))

(part-1 "input")

(defun close-stack (stack)
  (mapcar (lambda (c)
            (cond ((char= c #\() #\))
                  ((char= c #\[) #\])
                  ((char= c #\{) #\})
                  ((char= c #\<) #\>)))
          stack))

(let ((score (alexandria:alist-hash-table `((#\) . 1) (#\] . 2) (#\} . 3) (#\> . 4)))))
  (defun autocomplete-score (char)
    (gethash char score 0)))

(defun score-stack (stack)
  (iter
    (for c in stack)
    (for score initially 0 then (+ (autocomplete-score c) (* 5 score)))
    (finally (return score))))

(defun part-2 (file)
  (iter
    (for parsed-line in (mapcar #'parse-line (uiop:read-file-lines file)))
    (when (consp parsed-line)
      (collect (score-stack (close-stack parsed-line)) into scores))
    (finally (return (alexandria:median scores)))))

(part-2 "input")
