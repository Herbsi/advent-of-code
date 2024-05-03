(ql:quickload :str)
(ql:quickload :fset)
(ql:quickload :iterate)
(use-package :iter)

(defun read-graph (file)
  (let ((graph (make-hash-table :test #'equal)))
    (labels ((process-line (line)
               (destructuring-bind (v w) (str:split "-" line)
                 (setf (gethash v graph) (fset:with (or (gethash v graph) (fset:empty-set)) w)
                       (gethash w graph) (fset:with (or (gethash w graph) (fset:empty-set)) v)))))
      (iter
        (for line in (uiop:read-file-lines file))
        (process-line line)
        (finally (return graph))))))

(defun neighbors (graph vertex)
  (gethash vertex graph))

(defun small-cave-p (cave)
  (every #'lower-case-p cave))

(defun all-paths (graph start end)
  (labels ((generate-paths-to (end visited)
             (let ((visited (if (small-cave-p end) (fset:with visited end) visited)))
               (cond
                 ((equal start end) (fset:set nil))
                 ((fset:empty? (fset:set-difference (neighbors graph end) visited)) (fset:empty-set))
                 (t
                  (fset:reduce #'fset:union
                               (fset:image
                                (lambda (neighbor)
                                  (fset:image (lambda (path) (append path `(,neighbor)))
                                              (generate-paths-to neighbor visited)))
                                (fset:set-difference (neighbors graph end) visited))))))))
    (fset:image (lambda (path) (append path `(,end))) (generate-paths-to end (fset:empty-set)))))

(defun part-1 (file)
  (fset:set-size (all-paths (read-graph file) "start" "end")))

(part-1 "input")
 ; => 3576 (12 bits, #xDF8)

(defun part-2 (file)
  (let* ((graph (read-graph file))
         (caves (iter (for (cave nil) in-hashtable graph) (collect cave))))
    (iter
      ;; for unknow reasons, iter in-hashtable shits the bed if I put it here
      (for cave in caves)
      (for adjacent-caves = (neighbors graph cave))
      (when (and (not (string= cave "start")) (not (string= cave "end")) (small-cave-p cave))
        (for new-cave next (str:concat cave cave))

        ;; we temporarily duplicate one small cave
        (setf (gethash new-cave graph) adjacent-caves)
        (fset:do-set (neighbor adjacent-caves)
          (setf (gethash neighbor graph) (fset:with (neighbors graph neighbor) new-cave)))


        ;; We replace `new-cave' with `cave' in all paths so that duplicates get unioned out below
        (for the-paths next (fset:image (lambda (path) (substitute cave new-cave path :test #'string=)) (all-paths graph "start" "end")))
        (reducing the-paths by (lambda (expr acc) (fset:union expr acc)) into paths)

        ;; Remove all traces from `new-cave'
        (remhash new-cave graph)
        (fset:do-set (neighbor adjacent-caves)
          (setf (gethash neighbor graph) (fset:set-difference (neighbors graph neighbor) (fset:set new-cave)))))

      (finally (return (fset:set-size paths))))))

(part-2 "input")
 ; => 84271 (17 bits, #x1492F)
