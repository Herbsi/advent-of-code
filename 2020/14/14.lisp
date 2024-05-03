(ql:quickload :iterate)
(use-package :iter)

(ql:quickload :str)

(ql:quickload :cl-ppcre) 

(defun to-digits (n &optional (base 2))
  ;; returns a bit-vector of length 36
  ;; containing n in base digits
  ;; TODO make more functional
  (let ((digits (make-array 36 :initial-element 0)))
    (when (> n 0)
      (iter
        (for dix from (floor (log n base)) downto 0)
        (for exp from 0 to (floor (log n base)))
        (setf (aref digits (1- (- 36 exp)))
              (mod (floor (/ n (expt base exp))) base))))
    digits))

(defun from-digits (digits &optional (base 2))
  (iter
    (for idx from 0 to 35)
    (for exp from 35 downto 0)
    (sum (* (aref digits idx) (expt base exp)))))

(defun part-1 (instructions)
  (let ((mask nil)
        (memory (make-hash-table :test #'equal)))
    (labels ((mask (value)
               ;; guaranteed to be called when mask is not nil
               (from-digits
                (iter
                  (for digit in-vector (to-digits value))
                  (for char in-string mask)
                  (collect (if (char-equal char #\X)
                               digit
                               (read-from-string (string char)))
                    result-type vector))))
             (execute (instructions)
               (if instructions
                   (let* ((instr (car instructions))
                          (address-value (return-address-value instr)))
                     (if address-value
                         (setf (gethash (car address-value) memory)
                               (mask (cadr address-value)))
                         (setf mask (return-mask instr)))
                     (execute (cdr instructions)))
                   (iter
                     (for (nil val) in-hashtable memory)
                     (sum val)))))
      (execute instructions))))

(defun part-2 (instructions)
  (let ((mask nil)
        (memory (make-hash-table :test #'equal)))
    (labels ((mask (address)
               (iter
                (for digit in-vector (to-digits address))
                (for char in-string mask)
                (collect
                    (cond ((char= #\X char) #\X)
                          ((= 1 (read-from-string (string char))) #\1)
                          (t (digit-char digit)))
                  result-type string)))
             (memory-addresses (address)
               ;; recursively generate all addresses
               ;; if there still is an X, cut it out, solve recursively and
               ;; double result, inserting a 0 and a 1 for every recursively generated solution
               (let ((x-loc (search "X" address)))
                 (if x-loc
                     (mapcan (lambda (res) (list (str:insert "0" x-loc res)
                                                 (str:insert "1" x-loc res)))
                             (memory-addresses (str:join "" (list (str:substring 0 x-loc address)
                                                                  (str:substring (1+ x-loc) t address)))))
                     (list address))))
             (execute (instructions)
               (if instructions
                   (let* ((instr (car instructions))
                          (address-value (return-address-value instr)))
                     (if address-value
                         (iter
                          (for address in (memory-addresses (mask (car address-value))))
                          (setf (gethash address memory)
                                (cadr address-value)))
                         (setf mask (return-mask instr)))
                     (execute (cdr instructions)))
                   ;; Sum up all values stored in memory
                   (iter (for (nil val) in-hashtable memory) (sum val)))))
      (execute instructions))))

(defun return-address-value (mem-instruction)
  ;; Returns a list (a b) or NIL if mem-instruction is not a memomry instruction
  ;; a ... memory location
  ;; b ... value to write to that location
  (multiple-value-bind (string matches)
      (ppcre:scan-to-strings "mem\\[(\\d+)\\] = (\\d+)" mem-instruction)
    (map 'list #'read-from-string matches)))

(defun return-mask (mask-instruction)
  ;; Returns just the mask as a string
  (caddr (str:words mask-instruction)))

(format t "~a~%~a~%" (part-1 (uiop:read-file-lines "input.txt"))
        (part-2 (uiop:read-file-lines "input.txt"))) 
