(ql:quickload :fset)
(ql:quickload :iterate)
(use-package :iter)
(ql:quickload :alexandria)
(import 'alexandria:with-gensyms)

(defmacro start-of-*-marker (string n)
  (with-gensyms (i j window)
    `(iter
       (for ,i upfrom 1)
       (for ,j previous ,i back ,n initially 0)
       ;; Taking the window could be more efficient
       (for ,window = (subseq ,string ,j ,i))
       (until (= (fset:size (fset:convert 'fset:set ,window)) ,n))
       (finally (return ,i)))))

(defun start-of-packet-marker (string)
  (start-of-*-marker string 4))

(defun part-1 (file)
  (start-of-packet-marker (uiop:read-file-string file)))

(part-1 "input.txt") ; => 1544 (11 bits, #x608)

(defun start-of-message-marker (string)
  (start-of-*-marker string 14))

(defun part-2 (file)
  (start-of-message-marker (uiop:read-file-string file)))

(part-2 "input.txt") ; => 2145 (12 bits, #x861)
