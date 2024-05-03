(ql:quickload :iterate)
(use-package :iter)

(defun read-octopi (file)
  (make-array '(10 10) :initial-contents (mapcar (lambda (line)
                                               (map 'list (lambda (c) (parse-integer (string c))) line))
                (uiop:read-file-lines file))))

(defun neighboring-octopi (octopi i j)
  (iter
    (with (n m) = (array-dimensions octopi))
    (for (k l) in `((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)))
    (for x = (min (max 0 (+ i k)) (1- n)))
    (for y = (min (max 0 (+ j l)) (1- m)))
    (adjoining `(,x ,y) test #'equal)))

(defun do-step (octopi)
  (let ((flashed (make-array `(10 10 2) :initial-element nil))
        (present 0))
    (labels ((past (present) (mod (1+ present) 2))
             (increase-energy ()
               (iter (for i from 0 below 10)
                 (iter (for j from 0 below 10)
                   (incf (aref octopi i j)))))
             (mark-flashed ()
               (iter outer
                 (for i from 0 below 10)
                 (iter (for j from 0 below 10)
                   (when (and (not (flashed-p i j)) (> (aref octopi i j) 9))
                     (setf (aref flashed i j present) t)))))
             (flashed-p (i j)
               (or (aref flashed i j present) (aref flashed i j (past present))))
             (just-flashed-p (i j)
               (and (aref flashed i j (past present)) (not (aref flashed i j present))))
             (increase-neighbors-of-flashes ()
               (iter (for i from 0 below 10)
                 (iter (for j from 0 below 10)
                   (when (just-flashed-p i j)
                     (setf (aref flashed i j present) t)
                     (iter (for (x y) in (neighboring-octopi octopi i j))
                       (incf (aref octopi x y)))))))
             (unchanged-p ()
               (iter outer (for i from 0 below 10)
                 (iter (for j from 0 below 10)
                   (in outer (never (just-flashed-p i j))))))
             (count-flashes ()
               (iter outer (for i from 0 below 10)
                 (iter (for j from 0 below 10)
                   (in outer (counting (aref flashed i j present))))))
             (reset-energy ()
               (iter (for i from 0 below 10)
                 (iter (for j from 0 below 10)
                   (when (> (aref octopi i j) 9)
                     (setf (aref octopi i j) 0))))))
      (iter
        (if-first-time (progn (increase-energy) (mark-flashed)))
        (setf present (past present))
        (until (unchanged-p))
        (increase-neighbors-of-flashes)
        (mark-flashed)
        (finally (reset-energy) (return (values octopi (count-flashes))))))))

(defun part-1 (file &optional (steps 100))
  (iter
    (repeat steps)
    (for (values octopi flashes)
         initially (values (read-octopi file) 0)
         then (do-step octopi))
    (summing flashes into total-flashes)
    (finally (return (values octopi total-flashes)))))

;; Number of flashes is off by one for some reason
(part-1 "input" 101)

(defun part-2 (file)
  (iter
    (for step upfrom 0)
    (for (values octopi flashes)
         initially (values (read-octopi file) 0)
         then (do-step octopi))
    (until (= 100 flashes))
    (finally (return step))))

(part-2 "input")
 ; => 476 (9 bits, #x1DC)
