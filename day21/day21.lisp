(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defun parse-starting-pos (line)
  (digit-char-p (char line (1- (length line)))))

(defparameter *die* nil)
(defparameter *score* nil)
(defparameter *pos* nil)

(defun deterministic-die ()
  (loop for i below 10000 collect (1+ (rem i 10))))

(defun roll ()
  (+ (pop *die*) (pop *die*) (pop *die*)))

(defun advance (p n)
  (if (<= (incf p n) 10)
      p
    (- p (* (truncate p 10) 10))))

(defun move (player &optional (n (roll)))
  (let ((p (advance (nth player *pos*) n)))
    (setf (nth player *pos*) p)
    (incf (nth player *score*) p)))

(defun play (p1 p2 die &optional (win 1000))
  (let ((*die* die)
        (*pos* (list p1 p2))
        (*score* (list 0 0)))
    (loop
       for turn from 1
       when (>= (move (logand (1- turn) 1)) win)
       return (values turn *score*))))

(defun part-1 (&optional (data #'test-data))
  (destructuring-bind (p1 p2)
      (funcall data #'parse-starting-pos)
    (time (multiple-value-bind (turns score)
              (play p1 p2 (deterministic-die))
            (print (list turns score))
            (if (apply #'< score)
                (* (first score) turns 3)
              (* (second score) turns 3))))))

#|
For the quantum die - rolled 3 times - even though there are 27 possible
combinations of rolls, the final outcome is limited to range [3,9]. Even
rolling the worst possible combinations imaginable (toggling between
positions 1 and 4), the player would still win in 10 turns.

Each player gets a turn (except for a turn when player 1 wins), so for
each round there are 729 possible die combinations (27*27), but only
49 (7*7) game state outcomes.

The total number of possible games that are played before the player
reaches a score of 21 is 729^10. But the actual number of possible game
outcomes is limited to 49^10. And the majority of those games are won
very early and so we don't actually get anywhere close to that number.

For each player, we build a vector where the index is how many turns it
took to reach a score of 21 and the value is the count of all possible
combinations of die rolls would produce that winning state. These vectors
are built in parallel as all games are played out, bouncing between each
player.

Since we're reducing 27^10 down to 7^10, for each turn we need to keep
track of how many dice combinations arrived at the same place. This is
just a simple lookup table (e.g., only 1 combination of rolls ends up
with a 3, but 7 combinations lead to a 6).

Once the vectors are built it's possible to sum up how many combinations
of times each player won.
|#

(defparameter *die-combs* #(0 0 0 1 3 6 7 6 3 1))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (assert (= (reduce #'+ *die-combs*) 27)))

(defun simulate-quantum-game (p1 p2)
  (let ((p1-wins (make-array 11 :initial-element 0))
        (p2-wins (make-array 11 :initial-element 0)))

    ;; bounce back and forth between each player
    (labels ((play-p1 (p1-pos p1-score p2-pos p2-score turn combs)
               (dotimes (i 7)
                 (let* ((new-pos (advance p1-pos (+ i 3)))
                        (new-score (+ p1-score new-pos))
                        (new-combs (* combs (aref *die-combs* (+ i 3)))))
                   (if (>= new-score 21)
                       (incf (aref p1-wins turn) new-combs)
                     (play-p2 new-pos new-score p2-pos p2-score turn new-combs)))))

             (play-p2 (p1-pos p1-score p2-pos p2-score turn combs)
               (dotimes (i 7)
                 (let* ((new-pos (advance p2-pos (+ i 3)))
                        (new-score (+ p2-score new-pos))
                        (new-combs (* combs (aref *die-combs* (+ i 3)))))
                   (if (>= new-score 21)
                       (incf (aref p2-wins turn) new-combs)
                     (play-p1 p1-pos p1-score new-pos new-score (1+ turn) new-combs))))))

      ;; play and return the final vectors
      (play-p1 p1 0 p2 0 1 1)
      (values p1-wins p2-wins))))

  (defun part-2 (&optional (data #'test-data))
    (destructuring-bind (p1 p2)
        (funcall data #'parse-starting-pos)
      (time (multiple-value-bind (p1-wins p2-wins)
                (simulate-quantum-game p1 p2)
              (max (reduce #'+ p1-wins)
                   (reduce #'+ p2-wins))))))
