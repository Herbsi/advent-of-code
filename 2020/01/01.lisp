(ql:quickload :iterate)
(use-package :iter)
(ql:quickload :str)

(defun part-1 (numbers)
  (iter
    (with seen? = (make-hash-table))
    (for no in numbers)
    (when (gethash (- 2020 no) seen?)
      (return (* no (- 2020 no))))
    (setf (gethash no seen?) t)))


(defun part-2 (numbers)
  (iter outer
    (with seen? = (make-hash-table))
    (for i in numbers)
    (iter (for j in numbers)
      (when (gethash (- 2020 j) seen?)
        (return-from outer
          (apply #'* j (gethash (- 2020 j) seen?))))
      (setf (gethash (+ i j) seen?)
            (list i j)))))


(defun read-input (file)
  (mapcar #'read-from-string
          (str:split #\Newline (uiop:read-file-string file)
                     :omit-nulls t)))

