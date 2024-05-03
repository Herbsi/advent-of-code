(ql:quickload :str)
(ql:quickload :alexandria)
(use-package :alexandria)
(ql:quickload :iterate)
(use-package :iter)

(ql:quickload :cl-ppcre)

(defun read-file (file)
  (destructuring-bind (rules my-ticket tickets)
      (str:split "

" (uiop:read-file-string file))
    (values
     (mapcar #'read-rule (str:split #\Newline rules))
     (read-ticket (cadr (str:split #\Newline my-ticket)))
     (mapcar #'read-ticket (cdr (str:split #\Newline tickets :omit-nulls t))))))

(defun matches-rule? (ticket rule)
  (iter (for number in ticket)
    (thereis (funcall #'rule number))))

(defun read-rule (line)
  ;; returns a list of 3 items
  ;; the name of the rule as a symbol
  ;; and the two rules that define it as functions
  (multiple-value-bind (_ matches)
      (ppcre:scan-to-strings "(.*?): (\\d+)-(\\d+) or (\\d+)-(\\d+)" line)
    (when matches
      (list
       (aref matches 0)
       (lambda (no) (<= (read-from-string (aref matches 1))
                        no
                        (read-from-string (aref matches 2))))
       (lambda (no) (<= (read-from-string (aref matches 3))
                        no
                        (read-from-string (aref matches 4))))))))

(defun read-ticket (line)
  (mapcar #'read-from-string (str:split #\, line)))

(defun part-1 (tickets rules)
  (reduce #'+ (remove-if ;; remove all numbers that match at least 1 rule  
               (lambda (number)
                 (some (lambda (rule)
                         (funcall (fulfills-rule? rule) number))
                       rules))
               ;; flatten tickets
               (apply #'append tickets))))

(defun valid-ticket? (ticket rules)
  ;; valid if every ticket number matches some rule
  (every (lambda (number)
           (some (lambda (rule)
                   (funcall (fulfills-rule? rule) number))
                 rules))
         ticket))

(defun fulfills-rule? (rule)
  (lambda (number)
    (or (funcall (cadr rule) number)
        (funcall (caddr rule) number))))

(defun part-2 (valid-tickets rules)
  ;; returns a hash-map with rule-name as key
  ;; and a list of possible positions
  (let ((result (make-hash-table)))
    (iter (for rule in rules)
      (setf (gethash (car rule) result)
            (iter
              (for k from 0 to (1- (length (car valid-tickets))))
              (when (every
                     (lambda (ticket)
                       (funcall (fulfills-rule? rule) (nth k ticket)))
                     valid-tickets)
                (collect k))))
      (finally (return result)))))
;; TODO programmatically determine correct keys

(defun determine-indices (rules-mapping) '())

(defun calc-result (my-ticket rules-mapping)
  (apply #'* (mapcar (lambda (rule-name)
                       (nth (gethash rule-name rules-mapping) my-ticket))
                     '("departure location"
                       "departure station)"
                       "departure platform"
                       "departure track"
                       "departure date"
                       "departure time"))))

(multiple-value-bind (rules my-ticket tickets) (read-file "input.txt")
  (format t "~a~%~a~%" (part-1 tickets rules)
          (calc-result my-ticket
                       (part-2 (remove-if-not (lambda (ticket) (valid-ticket? ticket rules)) tickets)
                               rules))))
