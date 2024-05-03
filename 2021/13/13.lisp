(ql:quickload :str)
(ql:quickload :iterate)
(use-package :iter)
(ql:quickload :trivia)
(use-package :trivia)
(ql:quickload :trivia.ppcre)
(use-package :trivia.ppcre)
(ql:quickload :cl-ppcre)

(defun read-manual (file)
  (destructuring-bind (points folds) (str:split "

" (uiop:read-file-string file))
    (let* ((points (mapcar (lambda (line) (mapcar #'parse-integer (str:split "," line))) (str:split #\Newline points)))
           (max-x (apply #'max (mapcar #'first points)))
           (max-y (apply #'max (mapcar #'second points)))
           (manual (make-array `(,(1+ max-x) ,(1+ max-y)) :initial-element nil))
           (folds (mapcar #'parse-fold (str:split-omit-nulls #\Newline folds))))
      (iter (for (x y) in points)
        (setf (aref manual x y) t)
        (finally (return (list manual folds)))))))

(defun parse-fold (fold)
  (match fold
    ((ppcre "fold along x=(\\d+)" (read x)) `(x ,x))
    ((ppcre "fold along y=(\\d+)" (read y)) `(y ,y))))

(defun fold-axis (fold)
  (first fold))

(defun fold-coordinate (fold)
  (second fold))

(defun do-fold (manual fold)
  (cond ((eq (fold-axis fold) 'x)
         (let* ((x* (fold-coordinate fold))
                (m (array-dimension manual 0))
                (n (array-dimension manual 1))
                (new-manual (make-array `(,(- m x* 1) ,n) :initial-element nil)))
           ;; Here we fold
           (iter
             (for x from 1 to (min x* (- m x*)))
             (iter (for y from 0 below n)
               (setf (aref manual (- x* x) y) (or (aref manual (- x* x) y) (aref manual (+ x* x) y)))))
           ;; This is the part where the fold overlaps to the left
           (iter
             (for x from (1- (* 2 x*)) below m)
             (for new-x = (- m x 1))
             (iter (for y from 0 below n)
               (setf (aref new-manual new-x y)
                     (aref manual x y))))
           ;; These are the part where we folded
           (iter
             (for x from 0 below x*)
             (for new-x = (1- (+ x (max (- m (* 2 x*)) 0))))
             (iter (for y from 0 below n)
               (setf (aref new-manual new-x y)
                     (aref manual x y))))
           new-manual))
        ((eq (fold-axis fold) 'y)
         (let* ((y* (fold-coordinate fold))
                (m (array-dimension manual 0))
                (n (array-dimension manual 1))
                (new-manual (make-array `(,m ,(- n y* 1)) :initial-element nil)))
           ;; Here we fold
           (iter
             (for y from 1 to (min y* (- n y*)))
             (iter (for x from 0 below m)
               (setf (aref manual x (- y* y)) (or (aref manual x (- y* y)) (aref manual x (+ y* y))))))
           ;; This is the part where the fold overlaps to the left
           (iter
             (for y from (1- (* 2 y*)) below n)
             (for new-y = (- n y 1))
             (iter (for x from 0 below m)
               (setf (aref new-manual x new-y)
                     (aref manual x y))))
           ;; These are the part where we folded
           (iter
             (for y from 0 below y*)
             (for new-y = (1- (+ y (max (- n (* 2 y*)) 0))))
             (iter (for x from 0 below m)
               (setf (aref new-manual x new-y)
                     (aref manual x y))))
           new-manual))))

(defun print-manual (manual)
  (iter
    (with (n m) = (array-dimensions manual))
    (for y from 0 below m)
    (iter
      (for x from 0 below n)
      (format t "~a" (if (aref manual x y) "#" ".")))
    (format t "~%")))

(defun part-1 (file)
  (destructuring-bind (manual folds) (read-manual file)
    (iter outer
      (with folded-once = (do-fold manual (first folds)))
      (with (m n) = (array-dimensions folded-once))
      (for x from 0 below m)
      (iter (for y from 0 below n)
        (in outer (counting (aref folded-once x y)))))))

(part-1 "input")

(defun part-2 (file)
  (destructuring-bind (manual folds) (read-manual file)
    (print-manual (iter
                    (for fold in folds)
                    (for the-manual initially manual then (do-fold the-manual fold))
                    (finally (return the-manual))))))

(part-2 "input")
