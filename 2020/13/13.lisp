(ql:quickload :iterate)
(use-package :iter)
(ql:quickload :str)

(defun part-1 (earliest-departure bus-ids)
  (iter
    (for id in bus-ids)
    (finding id
             minimizing (- (* id (ceiling (/ earliest-departure id)))
                           earliest-departure)
             into (min-id min-value))
    (finally (return (* min-id min-value)))))

(defun extended-euclid (a b)
  ;; returns two numbers x & y such that ax + by = gcd(x, y)
  (labels ((iter (old-r r old-s s old-t tt)
             (if (zerop r)
                 (values old-s old-t)
                 (let ((quotient (floor (/ old-r r))))
                   (iter
                     r (- old-r (* quotient r))
                     s (- old-s (* quotient s))
                     tt (- old-t (* quotient tt)))))))
    (iter a b 1 0 0 1)))

(defun part-2 (bus-ids)
  (iter
    (with prod = (apply #'* (remove-if (lambda (ai) (eq 'x ai)) bus-ids)))
    (for k upfrom 0)
    (for id in bus-ids)
    (unless (eq id 'x)
      (for rest next (/ prod id))
      (multiple-value-bind (dk ek) (extended-euclid id rest)
        (sum (mod (* (- k) ek rest) prod) into result)))
    (finally (return (mod result prod))))) 

(with-open-file (stream "input.txt")
  (let ((earliest-departure (read-from-string (read-line stream)))
        (bus-ids (mapcar #'read-from-string (str:split #\, (read-line stream)))))
    (format t "~a~%~a~%"
            (part-1 earliest-departure (remove-if-not #'numberp bus-ids))
            (part-2 bus-ids)))) 
