(ql:quickload :iterate)
(use-package :iter)
(ql:quickload :alexandria)
(use-package :alexandria)

(defun read-heightmap (file)
  (let ((lines (mapcar (lambda (string) (map 'list (lambda (c) (parse-integer (string c))) string))
                       (uiop:read-file-lines file))))
    (make-array `(,(length lines) ,(length (car lines))) :initial-contents lines)))

(defun low-points (heightmap)
  (flet ((low-point-p (i j)
           (iter
             (with (n m) = (array-dimensions heightmap))
             (for (k l) in (map-product #'list '(-1 0 1) '(-1 0 1)))
             (for x = (min (max 0 (+ i k)) (1- n)))
             (for y = (min (max 0 (+ j l)) (1- m)))
             (unless (and (= x i) (= y j))
               (minimize (aref heightmap x y) into low-point))
             (finally (return (< (aref heightmap i j) low-point))))))
    (iter outer
      (with (n m) = (array-dimensions heightmap))
      (for i from 0 below n)
      (iter
        (for j from 0 below m)
        (when (low-point-p i j)
          (in outer (collect (list i j))))))))

(defun height (heightmap i j)
  (aref heightmap i j))

(defun part-1 (file)
  (iter
    (with heightmap = (read-heightmap file))
    (for low-point in (low-points heightmap))
    (summing (1+ (apply #'height heightmap low-point)))))

(part-1 "input")

(defun heightmap->graph (heightmap)
  "Store graph as node -> (list of neighbors) hashmap"
  (flet ((neighbors (i j)
           (iter
             (with (n m) = (array-dimensions heightmap))
             (for (k l) in `((-1 0) (0 -1) (1 0) (0 1)))
             (for x = (min (max 0 (+ i k)) (1- n)))
             (for y = (min (max 0 (+ j l)) (1- m)))
             (adjoining `(,x ,y) test #'equal))))
    (iter outer
      (with (n m) = (array-dimensions heightmap))
      (for i from 0 below n)
      (iter
        (for j from 0 below m)
        (unless (= 9 (height heightmap i j))
          (for true-neighbors = (remove-if (lambda (pos) (= 9 (apply #'height heightmap pos))) (neighbors i j)))
          (in outer (accumulate true-neighbors
                                by (lambda (expr acc) (progn (setf (gethash (list i j) acc) expr) acc))
                                initial-value (make-hash-table :test #'equal))))))))


(defun graph-neighbors (graph node)
  (gethash node graph))

(defun basin (graph low-point)
  "This is basically getting the connected component of low-point in the heightmap"
  (let ((marked (make-hash-table :test #'equal))
        (acc nil))
    (labels ((visit (node)
               (unless (gethash node marked)
                 (setf (gethash node marked) t
                       acc (cons node acc))
                 (iter
                   (for v in (graph-neighbors graph node))
                   (visit v)))))
      (visit low-point)
      acc)))

(defun part-2 (file)
  (let* ((heightmap (read-heightmap file))
         (graph-heightmap (heightmap->graph heightmap)))
    (apply #'*
           (mapcar #'length
                   (subseq
                    (sort
                     (mapcar (lambda (low-point) (basin graph-heightmap low-point)) (low-points heightmap))
                     (lambda (basin-1 basin-2) (>= (length basin-1) (length basin-2))))
                    0 3)))))

(part-2 "input")
