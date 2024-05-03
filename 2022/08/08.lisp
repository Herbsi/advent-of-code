(ql:quickload :iterate)
(use-package :iter)
(ql:quickload :str)
(ql:quickload :alexandria)

(defun parse-line (line)
  (map 'vector (lambda (char) (parse-integer (string char))) line))

(defun parse-input (file)
  (let ((lines (mapcar #'parse-line (uiop:read-file-lines file))))
    (make-array `(,(length lines) ,(length (car lines))) :initial-contents lines)))

(defun walk-forest (forest visible)
  (destructuring-bind (rows cols) (array-dimensions forest)
    (macrolet ((walk-axis (idx direction)
                 (alexandria:with-gensyms (i current-max tree)
                   `(iter
                      ,direction
                      ,(case direction
                         (:NS `(for ,i from 0 below rows))
                         (:WE `(for ,i from 0 below cols))
                         (:SN `(for ,i from rows downto 0))
                         (:EW `(for ,i from cols downto 0)))
                      (for ,tree = (aref forest
                                         ,@(if (member direction '(:NS :SN))
                                               `(,i ,idx)
                                               `(,idx ,i))))
                      (when (> ,tree ,current-max)
                        (setf (aref visible ,@(if (member direction '(:NS :SN))
                                                  `(,i ,idx)
                                                  `(,idx ,i)))
                              T))
                      (for ,current-max
                           initially -1
                           then (max ,current-max ,tree)))))
               (walk-directions ()
                 (alexandria:with-gensyms (direction idx)
                   `(progn
                      ,(iter
                         (for direction in '(:NS :WE :SN :EW))
                         (iter (for idx from 0 below (if (member direction '(:NS :SN)) cols rows))
                           (walk-axis idx direction))
                         (finally (return visible)))))))
      (walk-directions))))

(defun count-visible (visible)
  (iter outer
    (with (rows cols) = (array-dimensions visible))
    (for i from 0 below rows)
    (iter (for j from 0 below cols)
      (in outer (counting (aref visible i j))))))

(defun part-1 (file)
  (let* ((forest (parse-input file))
         (visible (make-array (array-dimensions forest) :initial-element nil))
         (visible (walk-forest forest visible)))
    (format t "~a" visible)
    (count-visible visible)))
