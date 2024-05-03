(ql:quickload :alexandria)
(ql:quickload :fset)
(ql:quickload :str)
(ql:quickload :iterate)
(use-package :iter)

(defun parse-display (line)
  (flet ((mkbag (list-of-strings)
           (fset:convert 'fset:bag (mapcar (lambda (string) (fset:convert 'fset:set string)) list-of-strings))))
    (destructuring-bind (patterns output) (str:split " | " line)
      `(,(mkbag (str:split " " patterns)) ,(str:split " " output)))))

(defun part-1 (file)
  (with-open-file (stream file)
    (iter
      (for line = (read-line stream nil 'eof))
      (until (eq line 'eof))
      (for (nil output) = (parse-display line))
      (summing (fset:count-if (lambda (output-value) (member (fset:set-size output-value) `(2 4 3 7))) output)))))

(part-1 "input")
 ; => 255 (8 bits, #xFF, #o377, #b11111111)

(defun solve-pattern (patterns)
  "Returs a segment to wire pairing, indicating for each segment which wire it is connected to

for examlpe \"abcdefg\" would be a “correct wiring” and \"deafgbc\" would be the wiring from the test case

We actually return a hashmap char->char, indictaing that !! segment y comes from wire x !! because
we want to reverse a scrambled output to the correct wire !!"
  (let ((intersections-of-size-n
          (iter
            (for size from 2 to 7)
            (collect (fset:filter (lambda (pattern) (= size (fset:set-size pattern))) patterns) into acc)
            (finally (return (map 'vector (lambda (set-of-patterns) (fset:reduce #'fset:intersection set-of-patterns)) acc)))))
        (known-segments (fset:empty-set))
        (segment->wire (make-hash-table :test #'equal)))
    (flet ((isec (n)
             ;; Returns all that characters that occur in *all* patterns of length n
             (aref intersections-of-size-n (- n 2))))
      (let ((a-segment (fset:arb (fset:set-difference (isec 3) (isec 2))))
            (d-segment (fset:arb (fset:intersection (isec 4) (isec 5))))
            (f-segment (fset:arb (fset:intersection (isec 2) (isec 6)))))
        (setf (gethash a-segment segment->wire) #\a
              (gethash d-segment segment->wire) #\d
              (gethash f-segment segment->wire) #\f
              known-segments (fset:union known-segments (fset:set a-segment d-segment f-segment))))
      (let ((c-segment (fset:arb (fset:set-difference (isec 2) known-segments)))
            (g-segment (fset:arb (fset:set-difference (isec 5) known-segments))))
        (setf (gethash c-segment segment->wire) #\c
              (gethash g-segment segment->wire) #\g
              known-segments (fset:union known-segments (fset:set c-segment g-segment))))
      (let ((b-segment (fset:arb (fset:set-difference (isec 4) known-segments))))
        (setf (gethash b-segment segment->wire) #\b
              known-segments (fset:union known-segments (fset:set b-segment))))
      (let ((e-segment (fset:arb (fset:set-difference (isec 7) known-segments))))
        (setf (gethash e-segment segment->wire) #\e
              known-segments (fset:union known-segments (fset:set e-segment))))
      (lambda (char) (gethash char segment->wire)))))

(defun decode-number (output-value wiring)
  (let ((number-from-standard-wiring
          (alexandria:alist-hash-table `(("abcefg" . 0)
                                         ("cf" . 1)
                                         ("acdeg" . 2)
                                         ("acdfg" . 3)
                                         ("bcdf" . 4)
                                         ("abdfg" . 5)
                                         ("abdefg" . 6)
                                         ("acf" . 7)
                                         ("abcdefg" . 8)
                                         ("abcdfg" . 9))
                                       :test #'equal)))
    (gethash (sort (map 'string wiring output-value) #'char<=) number-from-standard-wiring)))

(defun build-number (output wiring)
  (iter
    (for power downfrom (1- (length output)))
    (for output-value in output)
    (summing (* (expt 10 power) (decode-number output-value wiring)))))

(defun part-2 (file)
  (with-open-file (stream file)
    (iter
      (for line = (read-line stream nil 'eof))
      (until (eq line 'eof))
      (for (patterns output) = (parse-display line))
      (summing (build-number output (solve-pattern patterns))))))

(part-2 "input")
 ; => 982158 (20 bits, #xEFC8E)
