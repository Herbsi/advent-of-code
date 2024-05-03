(ql:quickload :fset)
(ql:quickload :str)
(ql:quickload :iterate)
(use-package :iter)

(defun read-input (file)
  "Returs a list of two lists, the decks of player 1 & 2 respectively,
already parsed into numbers."
  (mapcar #'(lambda (player-string)
              (mapcar #'read-from-string (rest (str:split-omit-nulls #\Newline
                                                                     player-string))))
          (str:split (str:concat (string #\Newline) (string #\Newline))
                     (uiop:read-file-string file))))


(defun mkcons (x)
  (if (consp x) x (list x)))


(defun play-round (player-1 player-2)
  "Returs a list of lists — either both players
or a list ((numbers) done) signalling the end"
  (cond ((and (first player-1) (first player-2))
         (if (> (first player-1) (first player-2))
             (list
              (append (rest player-1) (list (first player-1) (first player-2)))
              (mkcons (rest player-2)))
             (list
              (mkcons (rest player-1))
              (append (rest player-2) (list (first player-2) (first player-1))))))
        ((first player-1) `(,player-1 done))
        (t `(,player-2 done))))



(defun recursive-combat (player-1 player-2)
  "Takes two lists, the deck of player-1 and player-2, one of which might be (NIL)

Returns two values
-- a list of two lists, the new decks of player-{1,2} => (player-1 player-2)
-- a symbol — player-{1,2} or done-{1,2} — depending on the winner of the round;
done if the game is over"
  (let ((previous-rounds (fset:empty-set)))
    (labels ((play-round (player-1 player-2)
               ;; We’ve seen this round before
               (if (fset:contains? previous-rounds `(,player-1 ,player-2))
                   ;; So player 1 wins
                   (values player-1 'done-1)
                   (let ((top-1 (first player-1))
                         (top-2 (first player-2)))
                     ;; Save game so we don’t repeat ourselves
                     (fset:adjoinf previous-rounds `(,player-1 ,player-2))
                     (case (cond ((null top-1) 'done-2) ; player 1 is out of cards
                                 ((null top-2) 'done-1) ; player 2 is out of cards
                                 ;; We can recurse
                                 ((and (>= (length (rest player-1)) top-1)
                                       (>= (length (rest player-2)) top-2))
                                  (multiple-value-bind (result winner)
                                      (recursive-combat (subseq player-1 1 (1+ top-1))
                                                        (subseq player-2 1 (1+ top-2)))
                                    (case winner
                                      (done-1 'player-1)
                                      (done-2 'player-2))))
                                 ;; Can’t recurse, bigger card wins
                                 ((>= top-1 top-2) 'player-1)
                                 (t 'player-2))
                       (player-1
                        (play-round (append (rest player-1) `(,top-1 ,top-2))
                                    (mkcons (rest player-2))))
                       (player-2
                        (play-round (mkcons (rest player-1))
                                    (append (rest player-2) `(,top-2 ,top-1))))
                       (done-1
                        (values player-1 'done-1))
                       (done-2
                        (values player-2 'done-2)))))))
      (play-round player-1 player-2))))


(defun main (file)
  (iter
    (with players = (read-input file))
    (for result initially (apply #'play-round players)
         then (apply #'play-round result))
    (until (eq 'done (second result)))
    (finally (return
               (iter
                 (for i upfrom 1)
                 (for card in (reverse (first result)))
                 (sum (* i card)))))))


(defun main-2 (file)
  (iter
    (with players = (read-input file))
    (for card in (reverse (apply #'recursive-combat players)))
    (for i upfrom 1)
    (sum (* card i))))
