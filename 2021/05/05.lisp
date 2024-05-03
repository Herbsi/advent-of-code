(ql:quickload :str)
(ql:quickload :iterate)
(use-package :iter)

(defun parse-line-segment (string)
  (mapcar (lambda (s) (mapcar #'parse-integer (str:split "," s)))
          (str:split " -> " string)))

(defun read-line-segments (file)
  (mapcar #'parse-line-segment (uiop:read-file-lines file)))

(defun x1 (line-segment)
  (caar line-segment))

(defun x2 (line-segment)
  (caadr line-segment))

(defun y1 (line-segment)
  (cadar line-segment))

(defun y2 (line-segment)
  (cadadr line-segment))

(defun Δx (line-segment)
  (- (x2 line-segment) (x1 line-segment)))

(defun Δy (line-segment)
  (- (y2 line-segment) (y1 line-segment)))

(defun points-on-line-segment (line-segment)
  (let ((Δx (Δx line-segment))
        (Δy (Δy line-segment)))
    (if (= 0 Δx)
        (iter
          (for abs-y from 0 to (abs Δy))
          (for y = (* (signum Δy) abs-y))
          (collect `(,(x1 line-segment) ,(+ (y1 line-segment) y))))
        (iter
          (with slope =  (/ Δy Δx))
          ;; We do this so that the loop is never empty, even if Δx < 0
          ;; for x from 0 to -1 would not perform any iterations
          (for abs-x from 0 to (abs Δx))
          (for x = (* (signum Δx) abs-x))
          (collect `(,(+ (x1 line-segment) x) ,(+ (y1 line-segment) (* x slope))))))))

(defun mark-points (line-segment htable)
  (iter
    (for point in (points-on-line-segment line-segment))
    (if (gethash point htable)
        (incf (gethash point htable))
        (setf (gethash point htable) 1))
    (finally (return htable))))

(defun part-1 (file)
  (iter
    (for line-segment in (read-line-segments file))
    (for point-table
         initially (make-hash-table :test #'equal)
         ;; only consider horizontal or vertical lines
         then (if (member 0 `(,(Δx line-segment) ,(Δy line-segment)))
                  (mark-points line-segment point-table)
                  point-table))
    (finally (return
               (iter (for (nil count) in-hashtable point-table)
                 (counting (>= count 2)))))))

(part-1 "input")

(defun part-2 (file)
  (iter
    (for line-segment in (read-line-segments file))
    (for point-table
         initially (make-hash-table :test #'equal)
         ;; also consider diagonal lines
         then (mark-points line-segment point-table))
    (finally (return
               (iter (for (nil count) in-hashtable point-table)
                 (counting (>= count 2)))))))

(part-2 "input")
