(ql:quickload :str)
(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(use-package :iter)

(defun parse-stacks (lines &optional (number-of-stacks 9))
  (iter (with stacks = (make-array number-of-stacks :initial-element nil))
    (for line in lines)
    (for parsed-line = (parse-stack-line line))
    (iter
      (for crate in parsed-line)
      (for i upfrom 0)
      (when (not (str:blank? crate))
        (push (string (char crate 1)) (aref stacks i))))
    (finally (return (map 'vector
                          #'(lambda (stack)
                              ;; Some stacks endup with a redundant nil at the front
                              ;; Remove it
                              (reverse (if (not (car stack)) (cdr stack) stack)))
                          stacks)))))

(defun parse-stack-line (line)
  (cl-ppcre:all-matches-as-strings "(?:(\\[[A-Z]\\]|\\s{3}) ?)+?" line))

(defun source (instruction)
  (1- (car instruction)))

(defun destination (instruction)
  (1- (cadr instruction)))

(defun instruction-count (instruction)
  (third instruction))

(defun make-instruction (source destination count)
  (list source destination count))

(defun move-crate (instruction stacks)
  (iter (repeat (instruction-count instruction))
    (push (pop (aref stacks (source instruction)))
          (aref stacks (destination instruction)))
    (finally (return stacks))))

(defun parse-instruction-line (line)
  (multiple-value-bind (str matches)
      (cl-ppcre:scan-to-strings "move (\\d+) from (\\d+) to (\\d+)" line)
    (make-instruction (parse-integer (aref matches 1)) (parse-integer (aref matches 2)) (parse-integer (aref matches 0)))))

(defun collect-tops (stacks)
  (iter
    (for stack in-vector stacks)
    (accumulate (car stack) by #'(lambda (expr acc) (str:concat acc expr)) initial-value "" into result)
    (finally (return result))))

(defun part-1 (file &optional (number-of-stacks 9))
  (iter
    (with lines = (uiop:read-file-lines file))
    (with stack-instruction-split = (position "" lines :test #'string-equal))
    (for stacks
         initially (parse-stacks
                    (subseq lines 0 (1- stack-instruction-split))
                    number-of-stacks)
         then (move-crate instruction stacks))
    (for instruction in
         (mapcar #'parse-instruction-line (subseq lines (1+ stack-instruction-split))))
    (format t "~a~%~%" stacks)
    (finally (return (collect-tops stacks)))))

(part-1 "input.txt" 9)
 ; => "GRTSWNJHH"

(defun move-crate-2 (instruction stacks)
  (iter (repeat (instruction-count instruction))
    (collect (pop (aref stacks (source instruction))) into moving-crates)
    (finally
     (return
       (dolist (crate (nreverse moving-crates) stacks)
         (push crate (aref stacks (destination instruction))))))))

(defun part-2 (file &optional (number-of-stacks 9))
  (iter
    (with lines = (uiop:read-file-lines file))
    (with stack-instruction-split = (position "" lines :test #'string-equal))
    (for stacks
         initially (parse-stacks
                    (subseq lines 0 (1- stack-instruction-split))
                    number-of-stacks)
         then (move-crate-2 instruction stacks))
    (for instruction in
         (mapcar #'parse-instruction-line (subseq lines (1+ stack-instruction-split))))
    (finally (return (collect-tops stacks)))))

(part-2 "input.txt" 9)
 ; => "QLFQDBBHM"
