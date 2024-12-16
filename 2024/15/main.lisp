(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :str)
(ql:quickload :trivia)

(use-package :iter)


(defun parse-warehouse (string)
  (let* ((rows (mapcar (lambda (row) (coerce row 'list)) (str:lines string)))
         (n (length rows))
         (m (length (car rows)))
         (warehouse (make-array `(,n ,m) :initial-contents rows)))
    (list warehouse
          (iter outer
            (for i from 0 below n)
            (iter
              (for j from 0 below m)
              (in outer (finding `(,i ,j) such-that
                                 (char= (aref warehouse i j) #\@))))))))


(defun parse-instructions (string)
  (ppcre:regex-replace-all "\\s+" string ""))


(defun parse-input (filename)
  (destructuring-bind (warehouse instructions) (str:split "

" (uiop:read-file-string filename))
    `(,@(parse-warehouse warehouse)
      ,(parse-instructions instructions))))


(defun move-robot (warehouse robot move)
  (let* ((direction (trivia:match move
                      (#\< '(0 -1))
                      (#\^ '(-1 0))
                      (#\> '(0 +1))
                      (#\v '(+1 0))))
         (next-robot `(,(+ (car robot) (car direction)) ,(+ (cadr robot) (cadr direction)))))
    (trivia:match (iter
                    (for position
                         initially robot
                         then `(,(+ (car position) (car direction))
                                ,(+ (cadr position) (cadr direction))))
                    (for sight-line next (apply #'aref warehouse position))
                    (finding `(,position ,sight-line) such-that (member sight-line '(#\. #\#))))
      ((trivia:guard (list (list i j) #\.)
                     (and (= i (car next-robot))
                          (= j (cadr next-robot))))
       (setf (aref warehouse (car next-robot) (cadr next-robot)) #\@)
       (setf (aref warehouse (car robot) (cadr robot)) #\.)
       (list warehouse next-robot))
      ((list (list i j) #\.)
       (setf (aref warehouse (car next-robot) (cadr next-robot)) #\@)
       (setf (aref warehouse (car robot) (cadr robot)) #\.)
       (setf (aref warehouse i j) #\O)
       (list warehouse next-robot))
      ((list (list _ _) #\#)
       (list warehouse robot)))))


(defun gps-coordinate (box)
  (+ (* 100 (car box))
     (* 1 (cadr box))))


(defun part-1 (filename)
  (iter
    (with input = (parse-input filename))
    (for move in-string (caddr input))
    (for (warehouse robot)
         initially (subseq input 0 2)
         then (move-robot warehouse robot move))
    (finally
     (return
       (iter outer
         (with (n m) = (array-dimensions warehouse))
         (for i from 0 below n)
         (iter
           (for j from 0 below m)
           (when (char= (aref warehouse i j) #\O)
             (in outer (summing (gps-coordinate `(,i ,j)))))))))))


(part-1 "input.txt") ; => 1451928 (21 bits, #x162798)