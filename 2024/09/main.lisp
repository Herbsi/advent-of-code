(ql:quickload :iterate)
(ql:quickload :str)
(ql:quickload :trivia)

(use-package :iterate)

(defstruct file
  (id nil :type integer)
  (blocks nil :type integer))


(defstruct disk-space
  (blocks nil :type integer))


(defun parse-disk-map (disk-map)
  (let ((last-file (if (oddp (length disk-map))
                       (make-file :id (floor (/ (length disk-map) 2))
                                  :blocks (digit-char-p (char disk-map (1- (length disk-map)))))
                       nil)))
    (iter:iter
      (for id upfrom 0)
      (collect id)
      (for file-blocks in-string disk-map by 2)
      (for space-blocks in-string (str:substring 1 nil disk-map) by 2)
      (for file next (make-file :id id :blocks (digit-char-p file-blocks)))
      (for space next (make-disk-space :blocks (digit-char-p space-blocks)))
      (collect file into disk)
      (collect space into disk)
      (finally
       (return (if last-file (append disk (list last-file)) disk))))))


(defun move-block (sorted disk)
  (let ((back (alexandria:lastcar disk)))
    (trivia:match (list (car sorted) (car disk) back)
      ((trivia:guard (list
                      (file :id id-a :blocks blocks-a)
                      (file :id id-b :blocks blocks-b)
                      _)
                     (= id-a id-b))
       (list (cons (make-file :id id-a :blocks (+ blocks-a blocks-b)) (cdr sorted))
             (cdr disk)))
      ((trivia:guard (list _ front back)
                     (and (file-p front) (file-p back)))
       (list (cons front sorted) (cdr disk)))
      ((trivia:guard (list _ front back)
                     (and (disk-space-p front) (disk-space-p back)))
       (list sorted (butlast disk)))
      ((trivia:guard (list _ front back)
                     (and (file-p front) (disk-space-p back)))
       (list (cons front sorted) (cdr (butlast disk))))
      ((list _ (disk-space :blocks space-blocks) (file :id id :blocks file-blocks))
       (cond ((< file-blocks space-blocks)
              (list
               (cons back sorted)
               (cons (make-disk-space :blocks (- space-blocks file-blocks)) (cdr (butlast disk)))))
             ((= file-blocks space-blocks)
              (list (cons back sorted) (cdr (butlast disk))))
             ((> file-blocks space-blocks)
              (list
               (cons (make-file :id id :blocks space-blocks) sorted)
               (append (cdr (butlast disk)) (list (make-file :id id :blocks (- file-blocks space-blocks)))))))))))


(defun compact (disk-map)
  (iter
    (for (sorted disk) initially (list nil disk-map) then (move-block sorted disk))
    (while disk)
    (finally (return sorted))))


(defun checksum (disk)
  (iter
    (for file in disk)
    (for position initially 0 then (+ position (file-blocks file)))
    (sum (iter
           (for block-position from position below (+ position (file-blocks file)))
           (sum (* (file-id file) block-position))))))


(defun part-1 (filename)
  (checksum (reverse (compact (parse-disk-map (uiop:read-file-line filename))))))


(part-1 "input.txt")
                                        ; => 6607511583593 (43 bits, #x6026E617B69)
