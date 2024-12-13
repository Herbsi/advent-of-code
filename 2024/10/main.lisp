(ql:quickload :alexandria)
(ql:quickload :fset)
(ql:quickload :iterate)
(ql:quickload :str)

(use-package :iter)


(defun x-coord (position)
  (car position))


(defun y-coord (position)
  (cadr position))


(defun height (position)
  (caddr position))


(defun trailhead? (position)
  (= (height position) 0))


(defun make-position (x-coord y-coord height)
  (list x-coord y-coord height))


(defun parse-topographic-map (filename)
  (let* ((topographic-map (mapcar (lambda (line) (map 'list #'digit-char-p line)) (uiop:read-file-lines filename)))
         (rows (length topographic-map))
         (columns (length (car topographic-map)))
         (topographic-map (make-array `(,rows ,columns) :initial-contents topographic-map)))
    (flet ((edges (i j)
             (iter
               (with height = (aref topographic-map i j))
               (for delta-x in '(-1 0 +1))
               (appending
                (iter
                  (for delta-y in '(-1 0 +1))
                  (for x = (+ j delta-x))
                  (for y = (+ i delta-y))
                  (when (and (<= 0 x (1- columns)) ; check array bounds
                             (<= 0 y (1- rows))
                             (= (* delta-x delta-y) 0)) ; remove diagonal steps
                    (for neighbour-height next (aref topographic-map y x))
                    (when (= (- neighbour-height height) 1)
                      (collect (make-position x y neighbour-height)))))))))
      (iter outer
        (with graph = (make-hash-table :test #'equal))
        (for i from 0 below rows)
        (iter
          (for j from 0 below columns)
          (for height = (aref topographic-map i j))
          (for position = (make-position j i height))
          (setf (gethash position graph)
                (edges i j)))
        (finally (return-from outer graph)))))))


(defun score (graph trailhead)
  (let ((discovered (fset:empty-set))
        (peaks (fset:empty-set)))
    (labels ((dfs (v)
               (fset:adjoinf discovered v)
               (when (= (height v) 9)
                 (fset:adjoinf peaks v))
               (iter
                 (for w in (gethash v graph))
                 (unless (fset:member? w discovered)
                   (dfs w)))))
      (dfs trailhead))
    (fset:size peaks)))


(defun part-1 (filename)
  (iter
    (with graph = (parse-topographic-map filename))
    (with trailheads = (remove-if-not #'trailhead? (alexandria:hash-table-keys graph)))
    (for trailhead in trailheads)
    (summing (score graph trailhead))))


(part-1 "input.txt") ; => 778 (10 bits, #x30A)
