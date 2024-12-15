(ql:quickload :cl-ppcre)
(ql:quickload :iterate)

(use-package :iter)


(defvar *width* 101)
(defvar *height* 103)


(defun px (robot)
  (car robot))


(defun py (robot)
  (cadr robot))


(defun vx (robot)
  (caddr robot))


(defun vy (robot)
  (cadddr robot))


(defun make-robot (px py vx vy)
  (list px py vx vy))


(let ((ptrn (ppcre:create-scanner "p=(\\d+),(\\d+) v=(-?\\d+),(-?\\d+)")))
  (defun parse-robot (string)
    (ppcre:register-groups-bind
        ((#'parse-integer px py vx vy))
        (ptrn string)
      (make-robot px py vx vy))))


(defun move-robot (robot)
  (make-robot
   (mod (+ (px robot) (vx robot)) *width*)
   (mod (+ (py robot) (vy robot)) *height*)
   (vx robot)
   (vy robot)))


(defun safety-factor (robots)
  (let ((quadrant-1 (remove-if-not (lambda (robot)
                                     (and (<= 0 (px robot) (1- (/ (1- *width*) 2)))
                                          (<= 0 (py robot) (1- (/ (1- *height*) 2)))))
                                   robots))
        (quadrant-2 (remove-if-not (lambda (robot)
                                     (and (<= (/ (1+ *width*) 2) (px robot) (1- *width*))
                                          (<= 0 (py robot) (1- (/ (1- *height*) 2)))))
                                   robots))
        (quadrant-3 (remove-if-not (lambda (robot)
                                     (and (<= 0 (px robot) (1- (/ (1- *width*) 2)))
                                          (<= (/ (1+ *height*) 2) (py robot) (1- *height*))))
                                   robots))
        (quadrant-4 (remove-if-not (lambda (robot)
                                     (and (<= (/ (1+ *width*) 2) (px robot) (1- *width*))
                                          (<= (/ (1+ *height*) 2) (py robot) (1- *height*))))
                                   robots)))
    (* (length quadrant-1)
       (length quadrant-2)
       (length quadrant-3)
       (length quadrant-4))))


(defun part-1 (filename)
  (iter
    (for robots
         initially (mapcar #'parse-robot (uiop:read-file-lines filename))
         then (mapcar #'move-robot robots))
    (format t "~a~%" (length robots))
    (for i from 0 below 100)
    (finally (return (safety-factor robots)))))


(part-1 "input.txt") ; => 232253028 (28 bits, #xDD7E664)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-robots (robots)
  (iter
    (with robots = (sort (sort (copy-seq robots) #'< :key #'px) #'< :key #'py))
    (with row = 0)
    (with col = 0)
    (for robot in robots)
    (when (and (= row (py robot)) (= col (px robot)))
      (format t "#")
      (incf col))
    (when (and (= row (py robot)) (< col (px robot)))
      (format t "~a" (map 'string #'identity (iter (for i from col below (px robot)) (collect #\.))))
      (format t "#")
      (setf col (1+ (px robot))))
    (when (< row (py robot))
      (format t "~a~%" (map 'string #'identity (iter (for i from col below *width*) (collect #\.))))
      (iter (for i from row below (py robot))
        (format t "~a~%" (map 'string #'identity (iter (for i from 0 below *width*) (collect #\.)))))
      (setf row (py robot))
      (setf col (px robot))
      (format t "~a" (map 'string #'identity (iter (for i from 0 below col) (collect #\.))))
      (format t "#")
      (incf col))
    (when (= col *width*)
      (format t "~%")
      (setf col 0)
      (incf row))
    (finally (format t "~a~%" (map 'string #'identity (iter (for i from col below *width*) (collect #\.))))
             (iter
               (for row below *height*)
               (format t "~a~%" (map 'string #'identity (iter (for i from 0 below *width*) (collect #\.))))))))


(defun variance (robots)
  (let ((mean-x (/ (apply #'+ (mapcar #'px robots)) (length robots)))
        (mean-y (/ (apply #'+  (mapcar #'py robots)) (length robots)))
        (x2 (/ (apply #'+  (mapcar (lambda (x) (* x x)) (mapcar #'px robots))) (length robots)))
        (y2 (/ (apply #'+  (mapcar (lambda (y) (* y y)) (mapcar #'py robots))) (length robots))))
    (+ (- x2 (* mean-x mean-x))
       (- y2 (* mean-x mean-y)))))


(defun part-2 (filename)
  (with-open-file (*standard-output* "output.txt"
                                     :direction :output
                                     :if-exists :supersede)
    (iter
      (for robots
           initially (mapcar #'parse-robot (uiop:read-file-lines filename))
           then (mapcar #'move-robot robots))
      (for second from 0 below (* *width* *height*))
      (for metric = (variance robots))
      (format t "Second ~a ~a~%" second metric)
      (print-robots robots)
      (format t "~%")
      (minimize metric into optimised-metric)
      (finally (return optimised-metric)))))


(part-2 "input.txt") ; => 12783441/125000 (102.267525)
                                        ; occurs at 8179
