(ql:quickload :str)
(ql:quickload :trivia)
(use-package :trivia)
(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(use-package :iter)
(ql:quickload :alexandria)

(defun read-rule (string &optional (rule-set (make-hash-table :test #'equal)))
  (multiple-value-bind (string matches) (cl-ppcre:scan-to-strings "([A-Z][A-Z]) -> ([A-Z])" string)
    (setf (gethash (aref matches 0) rule-set) (aref matches 1))
    rule-set))

(defun read-file (file)
  (let ((lines (uiop:read-file-lines file)))
    (list (first lines)
          (iter (for line in (cddr lines))
            (reducing line by (lambda (acc expr) (read-rule expr acc)) initial-value (make-hash-table :test #'equal))))))

(defun step-polymer (polymer rule-set)
  (flet ((update-polymer (changes)
           ;; this assumes that the indices in changes are sorted from largest to smallest
           ;; that way, later changes don’t get invalidated when updating polymer
           (iter
             (for (i change) in changes)
             (setf polymer (str:concat (str:substring 0 (1+ i) polymer)
                                       change
                                       (str:substring (1+ i) nil polymer)))
             (finally (return polymer)))))
    (iter
      (with changes = nil)
      (for i from 0 below (1- (length polymer)))
      (for pair next (str:substring i (+ i 2) polymer))
      (if (gethash pair rule-set)
          (push `(,i ,(gethash pair rule-set)) changes))
      (finally
       (return (update-polymer changes))))))

(defun most-common (sequence)
  (iter
    (with counts = (make-hash-table :test #'equal))
    (for element in-sequence sequence)
    (if (gethash element counts)
        (incf (gethash element counts))
        (setf (gethash element counts) 1))
    (finally (return (iter (for (nil count) in-hashtable counts)
                       (maximize count))))))

(defun least-common (sequence)
  (iter
    (with counts = (make-hash-table :test #'equal))
    (for element in-sequence sequence)
    (if (gethash element counts)
        (incf (gethash element counts))
        (setf (gethash element counts) 1))
    (finally (return (iter (for (nil count) in-hashtable counts)
                       (minimize count))))))

(defun part-1 (file &optional (n 10))
  (destructuring-bind (polymer rule-set) (read-file file)
    (iter (repeat n)
      (for my-polymer initially polymer
           then (step-polymer my-polymer rule-set))
      (finally (return (- (most-common my-polymer) (least-common my-polymer)))))))



;; TODO needs smarter step-polymer
(defun part-2 (file)
  (part-1 file 40))

(defun new-segments (key rule-set)
  (let ((c (gethash key rule-set)))
    `(,(str:concat (str:s-first key) c)
      ,(str:concat c (str:s-last key)))))

(defun count-letters (start-segment rule-set steps counts)
  (labels ((rec (segment step)
             (when (and (/= step steps) (gethash segment rule-set))
               (if (gethash (gethash segment rule-set) counts)
                   (incf (gethash (gethash segment rule-set) counts))
                   (setf (gethash (gethash segment rule-set) counts) 1))
               (dolist (new-segment (new-segments segment rule-set))
                 (rec new-segment (1+ step))))))
    (if (gethash (str:s-first start-segment) counts)
        (incf (gethash (str:s-first start-segment) counts))
        (setf (gethash (str:s-first start-segment) counts) 1))
    (rec start-segment 0)
    counts))

(defun part-1-b (file &optional (steps 10))
  (destructuring-bind (string rule-set) (read-file file)
    (iter
      (for i from 0 to (- (length string) 2))
      (for substring = (str:substring i (+ i 2) string))
      (for counts initially (make-hash-table :test #'equal)
           then (count-letters substring rule-set steps counts))
      (finally (return (progn (incf (gethash (str:s-last string) counts)) counts))))))

(read-file "test")
(part-1-b "test" 40)
