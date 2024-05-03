(ql:quickload :cl-ppcre)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :trivia.ppcre)
(use-package :trivia)
(use-package :trivia.ppcre)

(defvar *rules* (make-hash-table) "The rule set")

(defun make-rule (rules-string arg)
  "Returns a nested (funcall (...)) expression that corresponds
to applying the rules in order, i.e. \"1 2\" yields
`(funcall (gethash 2 *rules*) (funcall (gethash 1 *rules*) ,arg))"
  (labels ((rec (rules)
             (if (= 1 (length rules))
                 `(funcall (gethash ,(first rules) *rules*) ,arg)
                 `(funcall (gethash ,(first rules) *rules*) ,(rec (rest rules))))))
    (rec (reverse (mapcar #'read-from-string (str:words rules-string))))))

(defun add-rule (rule-line)
  "Returns syntax for adding rule-line to *rules*"
  (let ((string-arg (gensym)))
    (match rule-line
      ((ppcre "(\\d+): \"(\\w+)\"" (read no) pre)
       `(setf (gethash ,no *rules*)
              (lambda (,string-arg)
                (and
                 (str:non-empty-string-p (str:prefix `(,,pre ,,string-arg)))
                 (str:replace-first ,pre "" ,string-arg)))))
      ;; Part 2 ONLY
      ("0: 8 11"
       `(setf (gethash 0 *rules*)
              (lambda (,string-arg)
                (funcall (gethash 8 *rules*)
                         (matches-at-end? 11 ,string-arg)))))
      ("8: 42 | 42 8"
       `(setf (gethash 8 *rules*)
              (lambda (,string-arg)
                (labels ((rec (,string-arg)
                           (let ((res (funcall (gethash 42 *rules*) ,string-arg)))
                             (cond ((and res (str:empty? res)) "")
                                   ((not res) nil)
                                   (t (rec res))))))
                  (rec ,string-arg)))))
      ("11: 42 31 | 42 11 31"
       `(setf (gethash 11 *rules*)
              (lambda (,string-arg)
                (labels ((rec (,string-arg)
                           (let ((res (matches-at-end?
                                       31 (funcall
                                           (gethash 42 *rules*) ,string-arg))))
                             (cond ((and res (str:empty? res)) "")
                                   ((not res) nil)
                                   (t (rec res))))))
                  (rec ,string-arg)))))
      ((ppcre "(\\d+): ([0-9 ]+) \\| ([0-9 ]+)" (read no) opt-1 opt-2)
       `(setf (gethash ,no *rules*)
              (lambda (,string-arg)
                (or ,(make-rule opt-1 string-arg)
                    ,(make-rule opt-2 string-arg)))))
      ((ppcre "(\\d+): ([0-9 ]+)" (read no) rules)
       `(setf (gethash ,no *rules*)
              (lambda (,string-arg)
                ,(make-rule rules string-arg)))))))

(defun matches-at-end? (rule-no str)
  "Returns the remaining string, if rule-no matches at the *end*, i.e.
if 2: \"a\" then (matches-at-end? 2 \"ba\") => \"b\""
  (labels ((rec (idx)
             (and (>= idx 0)
                  (or (matches-rule? rule-no (str:substring idx t str))
                      (rec (1- idx)))
                  (str:substring 0 idx str))))
    (and str (rec (1- (length str))))))


(defun get-rules-values (file)
  "Returns a list of two lists — the list of rules and the list of values"
  (mapcar #'(lambda (str) (str:split #\Newline str :omit-nulls t))
          (str:split "

" (uiop:read-file-string file))))

(defun get-rules (file)
  (first (get-rules-values file)))

(defun get-values (file)
  (second (get-rules-values file)))


(defmacro add-all-rules (file)
  `(progn ,@(mapcar #'add-rule (get-rules file))))


(defun test-rule (rule-no str)
  (funcall (gethash rule-no *rules*) str))

(defun matches-rule? (rule-no str)
  "Returns the rest of str if the beginning of str matches rule"
  (let ((result (test-rule rule-no str)))
    ;; a string is matched if the result is empty and not nil
    (and result (str:empty? result))))


(add-all-rules "test-2-after-update.txt")
(count-if #'(lambda (value) (matches-rule? 0 value))
          (get-values "test-2-after-update.txt"))
