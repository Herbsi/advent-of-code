(ql:quickload "trivia")
(use-package :trivia)
(ql:quickload "trivia.ppcre")
(use-package :trivia.ppcre)
(ql:quickload :cl-ppcre)

;; (defclass submarine ()
;;   ((horizontal-position :initarg :horizontal-position
;;                         :initform 0
;;                         :accessor :h-pos)
;;    (depth :initarg :depth
;;           :initform 0
;;           :accessor :depth)))

(defun h-pos (submarine)
  (car submarine))

(defun depth (submarine)
  (cadr submarine))

(defun aim (submarine)
  (caddr submarine))

(defun make-submarine (h-pos depth &optional (aim 0))
  (list h-pos depth aim))

(defun move (submarine command)
  (match command
    ((ppcre "forward (\\d+)" (read x))
     (make-submarine (+ x (h-pos submarine)) (depth submarine)))
    ((ppcre "down (\\d+)" (read z))
     (make-submarine (h-pos submarine) (+ z (depth submarine))))
    ((ppcre "up (\\d+)" (read z))
     (make-submarine (h-pos submarine) (- (depth submarine) z)))))

(defun move-2 (submarine command)
  (match command
    ((ppcre "forward (\\d+)" (read x))
     (make-submarine (+ x (h-pos submarine)) (+ (depth submarine) (* x (aim submarine))) (aim submarine)))
    ((ppcre "down (\\d+)" (read z))
     (make-submarine (h-pos submarine) (depth submarine) (+ z (aim submarine))))
    ((ppcre "up (\\d+)" (read z))
     (make-submarine (h-pos submarine) (depth submarine) (- (aim submarine) z)))))

(defun part-1 (file)
  (labels ((rec (submarine commands)
             (if (null commands)
                 (* (h-pos submarine) (depth submarine))
                 (rec (move submarine (car commands)) (cdr commands)))))
    (rec (make-submarine 0 0)
         (uiop:read-file-lines file))))

(defun part-2 (file)
  (labels ((rec (submarine commands)
             (if (null commands)
                 (* (h-pos submarine) (depth submarine))
                 (rec (move-2 submarine (car commands)) (cdr commands)))))
    (rec (make-submarine 0 0)
         (uiop:read-file-lines file))))

(part-1 "input")
 ; => 2150351 (22 bits, #x20CFCF)
(part-2 "input")
 ; => 1842742223 (31 bits, #x6DD603CF)
