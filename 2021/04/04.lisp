(ql:quickload :cl-herbsi)
(ql:quickload :str)
(ql:quickload :iterate)
(use-package :iter)

(defun read-rng (stream)
  (mapcar #'parse-integer (str:split "," (read-line stream))))

(defun read-board (stream)
  (flet ((read-board-line (stream)
           (str:split " " (str:trim (str:collapse-whitespaces (read-line stream))))))
    (progn
      (let* ((first-line (mapcar #'parse-integer (read-board-line stream)))
             (board-size (length first-line))
             (remaining-lines
               (iter (repeat (1- board-size))
                 (collect (mapcar #'parse-integer (read-board-line stream))))))
        (make-array `(,board-size ,board-size 2) :initial-contents (mapcar (lambda (row) (mapcar (lambda (el) `(,el nil)) row)) (cons first-line remaining-lines)))))))

(defun read-game (stream)
  (list (read-rng stream)
        (iter ;; Note that `peek-char' consumes the newlines between boards
          (until (eq 'eof (peek-char t stream nil 'eof)))
          (collect (read-board stream)))))

(defun play (number board)
  (flet ((mark (i j)
           (setf (aref board i j 1) t)))
    (iter
      (with (m n nil) = (array-dimensions board))
      (for i from 0 below m)
      (iter
        (for j from 0 below n)
        (when (= (aref board i j 0) number)
          (mark i j)))
      (finally (return board)))))

(defun win? (board)
  (iter
    ;; Assumes board is square!
    (with (m nil nil) = (array-dimensions board))
    (for i from 0 below m)
    (for (row-value col-value) next
         (iter
           (for j from 0 below m)
           (reducing (aref board i j 1) by (lambda (acc expr) (and acc expr)) into row-value)
           (reducing (aref board j i 1) by (lambda (acc expr) (and acc expr)) into col-value)
           (finally (return (list row-value col-value)))))
    (collect row-value into rows)
    (collect col-value into cols)
    (finally (return (or (some #'identity rows) (some #'identity cols))))))

(defun score (number board)
  (flet ((unmarked (i j) (not (aref board i j 1))))
    (* number
       (iter outer
         (with (m n nil) = (array-dimensions board))
         (for i from 0 below m)
         (iter
           (for j from 0 below n)
           (when (unmarked i j)
             (in outer (summing (aref board i j 0)))))))))

(defun part-1 (file)
  (with-open-file (stream file)
    (iter
      (with game = (read-game stream))
      (with numbers = (car game))
      (until (some #'identity (mapcar #'win? boards)))
      (for number in numbers)
      (for boards
           initially (cadr game)
           then (mapcar (lambda (board) (play number board)) boards))
      (finally
       (return (score number (find-if #'win? boards)))))))

(part-1 "input")
 ; => 11774 (14 bits, #x2DFE)

(defun part-2 (file)
  (with-open-file (stream file)
    (iter
      (with game = (read-game stream))
      (with numbers = (car game))
      (until (and (cl-herbsi:single? boards) (win? (car boards))))
      (for number in numbers)
      (for boards
           initially (cadr game)
           then (if (cl-herbsi:single? boards)
                    `(,(play number (car boards)))
                    (remove-if #'win? (mapcar (lambda (board) (play number board)) boards))))
      (finally
       (return (score number (car boards)))))))

(part-2 "input")
 ; => 4495 (13 bits, #x118F)
