(ql:quickload :alexandria)
(ql:quickload :fset)
(ql:quickload :iterate)
(ql:quickload :str)
(ql:quickload :trivia)

(use-package :iter)

(defun parse-garden (filename)
  (let* ((garden (mapcar (lambda (line) (map 'list #'identity line)) (uiop:read-file-lines filename)))
         (rows (length garden))
         (columns (length (car garden)))
         (garden (make-array `(,rows ,columns) :initial-contents garden)))
    (flet ((neighbours (i j)
             (iter outer
               (with (rows columns) = (array-dimensions garden))
               (for delta-i from -1 to +1)
               (iter (for delta-j from -1 to +1)
                 (for neighbour = `(,(+ i delta-i) ,(+ j delta-j)))
                 (when (and (<= 0 (car neighbour) (1- rows))
                            (<= 0 (cadr neighbour) (1- columns))
                            (< 0 (abs (+ delta-i delta-j)))
                            (zerop (* delta-i delta-j))
                            (char= (apply #'aref garden `(,i ,j)) (apply #'aref garden neighbour)))
                   (in outer (collect neighbour)))))))
      (iter
        (with graph = (make-hash-table :test #'equal))
        (for i from 0 below rows)
        (iter (for j from 0 below columns)
          (setf (gethash `(,i ,j) graph)
                (neighbours i j)))
        (finally (return `(,garden ,graph)))))))


(defun fences (garden graph i j)
  (- 4 (iter
         (with plant = (aref garden i j))
         (for neighbour in (gethash `(,i ,j) graph))
         (counting (char= (apply #'aref garden neighbour) plant)))))


(defun connected-components (graph)
  (let ((l nil)
        (not-visited (fset:convert 'fset:set (alexandria:hash-table-keys graph)))
        (components (make-hash-table :test #'equal))
        (rooted (fset:empty-set)))
    (labels ((visit (u)
               (when (fset:member? u not-visited)
                 (fset:removef not-visited u)
                 (iter
                   (for v in (gethash u graph))
                   (visit v))
                 (push u l)))
             (assign (u root)
               (unless (fset:member? u rooted)
                 (push u (gethash root components))
                 (fset:adjoinf rooted u)
                 (iter
                   (for v in (gethash u graph))
                   (assign v root)))))
      (iter
        (for u in (alexandria:hash-table-keys graph))
        (visit u))
      (iter
        (for u in l)
        (assign u u)
        (finally (return components))))))


(defun price (garden graph component)
  (flet ((fences (ij)
           (apply #'fences garden graph ij)))
    (reduce #'+ (mapcar #'fences component))))


(defun part-1 (filename)
  (destructuring-bind (garden graph) (parse-garden filename)
    (iter
      (for (root component) in-hashtable (connected-components graph))
      (summing (* (price garden graph component) (length component))))))


(part-1 "input.txt") ; => 1473276 (21 bits, #x167AFC)
