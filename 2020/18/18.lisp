(ql:quickload :iterate)
(use-package :iter)
(ql:quickload :str)
(ql:quickload :trivia)
(use-package :trivia)
(ql:quickload :trivia.ppcre)
(use-package :trivia.ppcre)
(ql:quickload :cl-ppcre)

(defun parse-input (string)
  (remove-if (lambda (x) (string-equal " " x))
             (str:split-omit-nulls "" string)))

;;; Part 1
(defun calculate-1 (list)
  (labels ((rec (list stack)
             (if (consp list)
                 (match (car list)
                   ((ppcre "([0-9]+)" (read n))
                    (match (car stack)
                      ((or '+ '*)
                       (rec (cdr list) (cons (funcall (car stack) n
                                                      (cadr stack))
                                             (cddr stack))))
                      ((or "(" nil)
                       (rec (cdr list) (cons n stack)))))
                   ((or "+" "*")
                    (rec (cdr list) (cons (read-from-string (car list)) stack)))
                   ("("
                    (rec (cdr list) (cons "(" stack)))
                   ;; if ")" then the stack looks like
                   ;; number "(" ..., so we remove the parenthesis
                   (")"
                    (rec
                     (cons (write-to-string (car stack)) (cdr list))
                     (cddr stack))))
                 (car stack))))
    (rec list nil)))

;;; Part 2
(defun infix->postfix (infix-list prec)
  (let ((stack nil)
        (result nil))
    (dolist (c infix-list)
      (match c
        ("(" (push c stack))
        ((ppcre "([0-9]+)" d) (push d result))
        ((or "+" "*")
         (iter
           (while (and (consp stack)
                       (>= (funcall prec (car stack))
                           (funcall prec c))))
           (push (pop stack) result))
         (push c stack))
        (")"
         (iter
           (until (string= "(" (car stack)))
           (push (pop stack) result)
           (finally (pop stack)))) ;; remove "("
        (" " nil)
        (otherwise (error "Invalid symbol INFIX->POSTFIX"))))
    (iter (while (consp stack))
      (push (pop stack) result)
      (finally (return (nreverse result))))))

(defun calculate (postfix-list)
  (let ((stack nil))
    (dolist (c postfix-list)
      (match c
        ((ppcre "([0-9]+)" (read d)) (push d stack))
        ((or "+" "*")
         (setf stack (cons (funcall (read-from-string c)
                                    (second stack) (first stack))
                           (cddr stack))))))
    (car stack)))

;;; Result

(defun part-1 (input)
  (labels ((prec (x)
             (match x
               ((or "+" "*") 1)
               (_ 0))))
    (iter (for line in-file input using #'read-line)
      (sum (calculate (infix->postfix (parse-input line)
                                      #'prec))))))

(defun part-2 (input)
  (labels ((prec (x) (match x
                       ("+" 2)
                       ("*" 1)
                       (_ 0))))
    (iter (for line in-file input using #'read-line)
      (sum (calculate (infix->postfix (parse-input line)
                                          #'prec))))))

(format t "~a~%~a~%" (part-1 "input.txt")
        (part-2 "input.txt"))
