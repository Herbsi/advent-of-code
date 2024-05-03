(ql:quickload :iterate)
(use-package :iter)
(ql:quickload :ppcre)
(ql:quickload :str)

(defun gen-intersection (list &rest lists)
  "Returns the intersection of list and all lists inside `lists'

i.e. (gen-intersection '(a b c) '(a b) '(a)) => '(a)"
  (labels ((rec (lists acc)
             (if (null lists)
                 acc
                 (rec (rest lists) (intersection (first lists) acc :test #'equal)))))
    (rec lists list)))


(defvar *allergen->foods* (make-hash-table :test #'equal)
  "A mapping from the allergens to all the foods that definitely contain it.")


(defvar *ingredient->allergen* nil
  "For each ingredient, return the allergen it contains.")


(defvar *all-foods* nil "A list of all foods")


(defvar *leftover-ingredients* nil "The ingredients without an allergen sofar.")


(defun remove-ingredient! (ingredient)
  "Updates *allergen->foods* so that ingredient is no longer present.

If `ingredient' was the last ingredient left for any particular food, that food is then completely removed,
i.e. no `nil' entries are left in *allergen->foods* values."
  (iter
   (for (allergen foods) in-hashtable *allergen->foods*)
   (for new-foods = (remove nil
                            (mapcar #'(lambda (food)
                                        (remove ingredient food :test #'equal))
                                    foods)))
   (setf (gethash allergen *allergen->foods*) new-foods)
   (setf *leftover-ingredients* (remove ingredient *leftover-ingredients*
                                        :test #'equal))))


(defun single-p (list)
  "Returs true if list contains exactly one element.  Doesn’t calculate the length first."
  (and (consp list) (not (rest list))))


(defun try-find-ingredient (allergen)
  "Tries to determine the ingredient containing `allergen'"
  (let ((candidates (apply #'gen-intersection (gethash allergen *allergen->foods*))))
    (when (single-p candidates)
      (first candidates))))


(defun parse-food (line)
  "Returns a list of two lists

-- the list of ingredients
-- the list of allergens"
  (mapcar #'str:words (str:split "contains" (ppcre:regex-replace-all "[(),]" line ""))))


(defun setup (file)
  "Sets up the program based on file."
  (clrhash *allergen->foods*)
  (setf *all-foods* nil)
  (setf *leftover-ingredients* nil)
  (setf *ingredient->allergen* nil)
  (iter (for line in (uiop:read-file-lines file))
    (destructuring-bind (food allergens) (parse-food line)
      (mapc #'(lambda (allergen)
                (setf (gethash allergen *allergen->foods*)
                      (cons food (gethash allergen *allergen->foods*))))
            allergens)
      (mapc #'(lambda (ingredient)
                (setf *leftover-ingredients*
                      (union (list ingredient) *leftover-ingredients* :test #'equal)))
            food)
      (setf *all-foods* (cons food *all-foods*)))))


(defun sort-by-value (alist)
  "Sorts alist by (string-) value

i.e. (sort-by-value '((a . \" b\") (b . \"a\")) => '((b . \"a\") (a . \"b\"))"
  (sort alist #'(lambda (left right) (string< (cdr left) (cdr right)))))


(defun main ()
  (iter (while (> (hash-table-count *allergen->foods*) 0))
    (iter (for (allergen nil) in-hashtable *allergen->foods*)
      (for candidate = (try-find-ingredient allergen))
      (when candidate
        (remove-ingredient! candidate)
        (remhash allergen *allergen->foods*)
        (push (cons candidate allergen) *ingredient->allergen*)
        (return))))
  (format t "~a~%~{~a~^,~}~%"
          (reduce #'+ (mapcar #'(lambda (ingredient)
                                  (count ingredient (apply #'append *all-foods*)
                                         :test #'equal))
                              *leftover-ingredients*))
          (mapcar #'car (sort-by-value *ingredient->allergen*))))

