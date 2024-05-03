(ql:quickload :str)

(defun parse-input (file &optional (parse-line #'parse-line-1))
  (mapcar (lambda (line) (funcall parse-line line)) (uiop:read-file-lines file)))

(defun parse-line-1 (line)
  (mapcar #'(lambda (str) (decode (char str 0)))
          (str:split " " line)))

(defun parse-line-2 (line)
  (mapcar #'(lambda (str) (decode-2 (char str 0)))
          (str:split " " line)))

(defun decode-1 (letter)
  (cond ((member letter '(#\A #\X)) 'rock)
        ((member letter '(#\B #\Y)) 'paper)
        ((member letter '(#\C #\Z)) 'scissor)))

(defun decode-2 (letter)
  (cond ((eq letter #\A) 'rock)
        ((eq letter #\B) 'paper)
        ((eq letter #\C) 'scissor)
        ((eq letter #\X) 'loss)
        ((eq letter #\Y) 'draw)
        ((eq letter #\Z) 'win)))

(defun shape-score (shape)
  (cond ((eq shape 'rock) 1)
        ((eq shape 'paper) 2)
        ((eq shape 'scissor) 3)))

(defun outcome-score (outcome)
  (cond ((eq outcome 'loss) 0)
        ((eq outcome 'draw) 3)
        ((eq outcome 'win) 6)))

(defun decode-shape (shape)
  (cond ((= shape 0) 'rock)
        ((= shape 1) 'paper)
        ((= shape 2) 'scissor)))

(defun encode-shape (shape)
  (cond ((eq shape 'rock) 0)
        ((eq shape 'paper) 1)
        ((eq shape 'scissor) 2)))

(defun encode-outcome (outcome)
  (cond ((eq outcome 'draw) 0)
        ((eq outcome 'win) 1)
        ((eq outcome 'loss) 2)))

(defun player (round)
  (cadr round))

(defun opponent (round)
  (car round))

(defun target-outcome (round)
  (cadr round))

(defun outcome (round)
  (let ((diff (mod (- (encode-shape (player round)) (encode-shape (opponent round))) 3)))
    (cond ((= diff 0) 'draw)
          ((= diff 1) 'win)
          ((= diff 2) 'loss))))

(defun score-round (round)
  (+ (shape-score (player round)) (outcome-score (outcome round))))

(defun part-1 (file)
  (apply #'+ (mapcar #'score-round (parse-input file))))

(part-1 "input.txt")
 ; => 13268 (14 bits, #x33D4)

(defun strategy (round)
  (decode-shape (mod (+ (encode-shape (opponent round))
                        (encode-outcome (target-outcome round)))
                     3)))

(defun score-round-2 (round)
  (let ((player (strategy round))
        (outcome (target-outcome round)))
    (+ (shape-score player) (outcome-score outcome))))

(defun part-2 (file)
  (apply #'+ (mapcar #'score-round-2 (parse-input file #'parse-line-2))))

(part-2 "input.txt")
 ; => 15508 (14 bits, #x3C94)
