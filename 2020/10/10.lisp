(ql:quickload :str)
(ql:quickload :iterate)
(use-package :iter)

(defun read-adapters (file)
  (let ((sorted (sort (mapcar #'read-from-string
                              (str:split #\Newline (uiop:read-file-string file)
                                         :omit-nulls t))
                      #'<)))
    `(0 ,@sorted ,(+ 3 (apply #'max sorted)))))

(defun part-1 (adapters)
  (*
   (count 1 (mapcar #'- (rest adapters) adapters))
   (count 3 (mapcar #'- (rest adapters) adapters))))

(defun count-valid-arrangements (adapters)
  "Returns the number of valid ways, adapters canb arranged.

It runs in linear time (wrt to the number of adapters) thanks to the following observation.

If a_n is the nth adapter, then the number of valid arrangements ending in a_n is the sum of all arrangemens ending in
an adapter that could come before a_n; i.e.
f(a_n) = f(a_{n-1}) + f(a_{n-2}) + f(a_{n-3}); but the two adapters two and three steps back are only used if their jolt difference to a_n is
less than or equal to 3."
  (iter
    (for prev-3 initially -4 then prev-2)
    (for prev-2 initially -4 then prev-1)
    (for prev-1 initially 0 then curr)
    (for curr in (cdr adapters))
    (for no-of-possible-pred =
         (count-if #'(lambda (pred) (<= (- curr pred) 3)) `(,prev-1 ,prev-2 ,prev-3)))
    (for acc initially '(1 0 0) then
         (cons (apply #'+ (subseq acc 0 no-of-possible-pred))
               (subseq acc 0 2)))
    (finally (return (first acc)))))


