(ql:quickload :esrap)

(defstruct file
  (name "" :type string)
  (size 0 :type integer))

(defstruct dir
  (name "" :type string)
  (files nil :type list)
  (children nil :type list))

(defun walk-directory (directory reducer initial-result)
  (let ((result initial-result))
    (labels ((walk (directory)
               (setf result (funcall reducer result directory))
               (dolist (child dir-children result)
                 (walk child))))
      (walk directory))))

(make-dir :name "/")
(esrap:defrule command "$ " :)
(esrap:parse 'command "$ cd /")
(esrap:defrule dir "dir ")
