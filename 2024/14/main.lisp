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
    (for i from 0 below 100)
    (finally (return (safety-factor robots)))))


(part-1 "input.txt") ; => 232253028 (28 bits, #xDD7E664)
