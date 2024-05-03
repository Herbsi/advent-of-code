(ql:quickload :cl-utilities)
(use-package :cl-utilities)
(ql:quickload :misc-extensions)
(use-package :new-let)

(defparameter *moves* 100)
(defparameter *input* '(4 6 7 5 2 8 1 9 3))
(defparameter *test* '(3 8 9 1 2 5 4 6 7))
(defvar *circle* nil)

;; As an invariant, keep current cup at the front of *circle* at all times

(defun pick-up-cups ()
  "Selects the three cups after the current cup"
  (subseq *circle* 1 4))


(defun find-dest (current-cup picked-up-cups)
  "Returns the position in the current *circle* of the destination cup"
  ;; The destination is either the smallest cup less than the first picked-up cup
  ;; or the max of the remaining cups
  (let ((*circle* (set-difference *circle* (append picked-up-cups `(,current-cup)))))
    (apply #'max (or (remove-if (fn (cup) (>= cup current-cup)) *circle*)
                     `(,(apply #'max *circle*))))))


;; TODO the next starting cup is not dest
;; but the cup after current-cup in the new *circle*
(defun new-circle (current-cup dest-cup picked-up-cups)
  (let* ((front `(,dest-cup ,@picked-up-cups))
         (rem-part (mapcar (fn (part) (remove-if (fn (cup)
                                                   (member cup (cons current-cup front)))
                                                 part))
                           (split-sequence dest-cup *circle*)))
         (middle (second rem-part))
         (back (first rem-part)))
    (format t "~a~%" rem-part)
    (append front middle back (list current-cup))))


(defun move () 
  (format t "~&cups: ~{~a~^ ~}~%" *circle*)
  (let* ((current-cup (first *circle*))
         (picked-up-cups (pick-up-cups))
         (dest-cup (find-dest current-cup picked-up-cups)))
    (format t "~&pick up: ~{~a~^, ~}~%dest: ~a~%~%" picked-up-cups dest-cup)
    (setf *circle* (new-circle current-cup dest-cup picked-up-cups))))


(defun main (&optional (*circle* *input*) (*moves* *moves*))
  (dotimes (i *moves*
              (format nil "~{~a~}" *circle*))
    (move)))
