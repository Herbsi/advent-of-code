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
  (iter:iter
    (with disk-map = (map 'vector #'digit-char-p disk-map))
    (with disk = (make-hash-table))

    (for file-id upfrom 0)
    (for file-blocks in-vector disk-map by 2)
    (for space-blocks in-vector (subseq disk-map 1) by 2)

    (for file next (make-file :id file-id :blocks file-blocks))
    (for disk-space next (make-disk-space :blocks space-blocks))

    (for file-position initially 0 then (+ file-position file-blocks space-blocks))
    (for space-position first file-blocks then (+ file-position file-blocks))

    (setf (gethash file-position disk) file)
    (collect (list file-position file) into file-queue at beginning)

    (when (> space-blocks 0)
      (setf (gethash space-position disk) disk-space)
      (collect (list space-position disk-space) into disk-space-queue))

    (finally
     (let* ((blocks (aref disk-map (1- (length disk-map))))
            (file (make-file :id file-id :blocks blocks))
            (position (+ file-position space-blocks)))
       (setf (gethash position disk) file)
       (push (list position file) file-queue))
     (return (list disk file-queue disk-space-queue)))))


(defun move-block (disk file-queue disk-space-queue)
  (trivia:match (list (car file-queue) (car disk-space-queue))
    ((list (list file-position (file :id file-id :blocks file-blocks))
           (list space-position (disk-space :blocks space-blocks)))
     (if (< file-position space-position)
         (progn
           (remhash space-position disk)
           (list disk (cdr file-queue) (cdr disk-space-queue)))
         (progn
           (remhash file-position disk)
           (cond ((< file-blocks space-blocks)
                  (let ((new-space-position (+ space-position file-blocks))
                        (remaining-space (make-disk-space :blocks (- space-blocks file-blocks))))
                    (setf (gethash space-position disk) (make-file :id file-id :blocks file-blocks))
                    (setf (gethash new-space-position disk) remaining-space)
                    (list disk
                          (cdr file-queue)
                          (cons (list new-space-position remaining-space) (cdr disk-space-queue)))))
                 ((= file-blocks space-blocks)
                  (setf (gethash space-position disk) (make-file :id file-id :blocks file-blocks))
                  (list disk
                        (cdr file-queue)
                        (cdr disk-space-queue)))
                 ((> file-blocks space-blocks)
                  (let ((remaining-file (make-file :id file-id :blocks (- file-blocks space-blocks)))
                        (moved-file (make-file :id file-id :blocks space-blocks)))
                    (setf (gethash file-position disk) remaining-file)
                    (setf (gethash space-position disk) moved-file)
                    (list
                     disk
                     (cons (list file-position remaining-file) (cdr file-queue))
                     (cdr disk-space-queue))))))))))


(defun compact (disk file-queue disk-space-queue)
  (labels ((rec (disk file-queue disk-space-queue)
             (if disk-space-queue
                 (apply #'rec (move-block disk file-queue disk-space-queue))
                 disk)))
    (rec disk file-queue disk-space-queue)))


(defun checksum (disk)
  (iter
    (for (position file) in-hashtable disk)
    (when (file-p file)
      (sum (* (file-id file)
              (file-blocks file)
              (+ position (/ (1- (file-blocks file)) 2)))))))


(defun part-1 (filename)
  (checksum (apply #'compact (parse-disk-map (uiop:read-file-line filename)))))


(part-1 "input.txt") ; => 6607511583593 (43 bits, #x6026E617B69)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun move-block-2 (disk file-queue disk-space-queue)
  (trivia:match (car file-queue)
    ((list file-position (file :id file-id :blocks file-blocks))
     (alexandria:if-let ((idx
                          (position-if (lambda (disk-space)
                                         (and
                                          (< (car disk-space) file-position)
                                          (<= file-blocks (disk-space-blocks (cadr disk-space)))))
                                       disk-space-queue)))
       (trivia:match (nth idx disk-space-queue)
         ((list space-position (disk-space :blocks space-blocks))
          (remhash file-position disk)
          (cond ((< file-blocks space-blocks)
                 (let ((new-space-position (+ space-position file-blocks))
                       (remaining-space (make-disk-space :blocks (- space-blocks file-blocks))))
                   (setf (gethash space-position disk) (make-file :id file-id :blocks file-blocks))
                   (setf (gethash new-space-position disk) remaining-space)
                   (list disk
                         (cdr file-queue)
                         (append (subseq disk-space-queue 0 idx)
                                 `(,(list new-space-position remaining-space))
                                 (subseq disk-space-queue (1+ idx) nil)))))
                ((= file-blocks space-blocks)
                 (setf (gethash space-position disk) (make-file :id file-id :blocks file-blocks))
                 (list disk
                       (cdr file-queue)
                       (append (subseq disk-space-queue 0 idx)
                               (subseq disk-space-queue (1+ idx) nil)))))))
       (list disk (cdr file-queue) disk-space-queue)))))


(defun compact-2 (disk file-queue disk-space-map)
  (labels ((rec (disk file-queue disk-space-map)
             (if file-queue
                 (apply #'rec (move-block-2 disk file-queue disk-space-map))
                 disk)))
    (rec disk file-queue disk-space-map)))


(defun part-2 (filename)
  (checksum (apply #'compact-2 (parse-disk-map (uiop:read-file-line filename)))))


(part-2 "input.txt") ; => 6636608781232 (43 bits, #x60934B57BB0)
