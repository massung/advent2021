(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defparameter *command-pattern*
  #r"(on|off) x=([^.]+)%.%.([^,]+),y=([^.]+)%.%.([^,]+),z=([^.]+)%.%.([^,]+)")

(defstruct pt x y z)
(defstruct box min max)

(defun parse-command (line)
  (with-re-match (m (match-re *command-pattern* line))
    (list (equal $1 "on")
          (make-pt :x (parse-integer $2)
                   :y (parse-integer $4)
                   :z (parse-integer $6))
          (make-pt :x (parse-integer $3)
                   :y (parse-integer $5)
                   :z (parse-integer $7)))))

(defun intersects-p (a b)
  (not (or (< (pt-x (box-max a)) (pt-x (box-min b)))
           (> (pt-x (box-min a)) (pt-x (box-max b)))
           (< (pt-y (box-max a)) (pt-y (box-min b)))
           (> (pt-y (box-min a)) (pt-y (box-max b)))
           (< (pt-z (box-max a)) (pt-z (box-min b)))
           (> (pt-z (box-min a)) (pt-z (box-max b))))))

(defun encloses-p (a b)
  (and (<= (pt-x (box-min a)) (pt-x (box-min b)))
       (>= (pt-x (box-max a)) (pt-x (box-max b)))
       (<= (pt-y (box-min a)) (pt-y (box-min b)))
       (>= (pt-y (box-max a)) (pt-y (box-max b)))
       (<= (pt-z (box-min a)) (pt-z (box-min b)))
       (>= (pt-z (box-max a)) (pt-z (box-max b)))))

(defun intersecting-box (a b)
  (let ((min (make-pt :x (max (pt-x (box-min a)) (pt-x (box-min b)))
                      :y (max (pt-y (box-min a)) (pt-y (box-min b)))
                      :z (max (pt-z (box-min a)) (pt-z (box-min b)))))
        (max (make-pt :x (min (pt-x (box-max a)) (pt-x (box-max b)))
                      :y (min (pt-y (box-max a)) (pt-y (box-max b)))
                      :z (min (pt-z (box-max a)) (pt-z (box-max b))))))
    (make-box :min min :max max)))

(defun partitions (box)
  (with-slots ((min-x x) (min-y y) (min-z z)) (box-min box)
    (with-slots ((max-x x) (max-y y) (max-z z)) (box-max box)
      (let* ((left (1- min-x))
             (right (1+ max-x))
             (bottom (1- min-y))
             (top (1+ max-y))
             (front (1- min-z))
             (back (1+ max-z))

             ;; all the possible partition ranges in each dimension
             (xs `((,most-negative-fixnum ,left) (,min-x ,max-x) (,right ,most-positive-fixnum)))
             (ys `((,most-negative-fixnum ,bottom) (,min-y ,max-y) (,top ,most-positive-fixnum)))
             (zs `((,most-negative-fixnum ,front) (,min-z ,max-z) (,back ,most-positive-fixnum)))

             ;; end result
             (partitions nil))
        (loop for x from 0 and (min-x max-x) in xs do
             (loop for y from 0 and (min-y max-y) in ys do
                  (loop for z from 0 and (min-z max-z) in zs unless (= x y z 1) do
                       (let ((part (make-box :min (make-pt :x min-x :y min-y :z min-z)
                                             :max (make-pt :x max-x :y max-y :z max-z))))
                         (push part partitions)))))

        ;; all 26 surrounding partitions
        partitions))))

(defun intersecting-partitions (a b)
  (loop
     for partition in (partitions a)
     when (intersects-p partition b)
     collect (intersecting-box partition b)))

(defun union-box (boxes b)
  (let ((i (position b boxes :test #'intersects-p)))
    (cond
      ;; no overlaps, just add this new box to the list
      ((null i)
       (vector-push-extend b boxes))

      ;; the new box is completely enclosed by another, do nothing
      ((encloses-p (aref boxes i) b))

      ;; add each of the surrounding partitions
      (t (dolist (part (partitions (aref boxes i)))
           (when (intersects-p part b)
             (union-box boxes (intersecting-box part b))))))))

(defun difference-box (boxes b)
  (let ((rm nil))

    ;; remove all overlapping boxes from the list
    (do ((i 0))
        ((>= i (length boxes)))
      (let ((a (aref boxes i)))
        (if (intersects-p a b)
            (progn
              (unless (encloses-p b a)
                (push a rm))
              (setf (aref boxes i) (vector-pop boxes)))
          (incf i))))

    ;; for each of the overlapping add (up to) 26 partitions back
    (dolist (part (partitions b))
      (dolist (a rm)
        (when (intersects-p part a)
          (vector-push-extend (intersecting-box part a) boxes))))))

(defun run-commands (commands)
  (let ((boxes (make-array 1000 :adjustable t :fill-pointer 0))

        ;; find the first command that is an `on` and add that box
        (root (loop for (c min max) = (pop commands) when c return (make-box :min min
                                                                             :max max))))

    ;; start with only the root box
    (vector-push-extend root boxes)

    ;; process all the other commands
    (loop
       for (cmd min max) in commands
       for box = (make-box :min min :max max)
       do (if cmd
              (union-box boxes box)
            (difference-box boxes box))
       finally (return boxes))))

(defun volume (box)
  (* (1+ (- (pt-x (box-max box)) (pt-x (box-min box))))
     (1+ (- (pt-y (box-max box)) (pt-y (box-min box))))
     (1+ (- (pt-z (box-max box)) (pt-z (box-min box))))))

(defun part-1 (&optional (data #'test-data))
  (let ((commands (funcall data #'parse-command)))
    (time (let* ((boxes (run-commands commands))
                 (limited (let ((region (make-box :min (make-pt :x -50 :y -50 :z -50)
                                                  :max (make-pt :x +50 :y +50 :z +50))))
                            (loop
                               for box across boxes
                               when (intersects-p box region)
                               collect (intersecting-box box region)))))
            (reduce #'+ (mapcar #'volume limited))))))

(defun part-2 (&optional (data #'test-data))
  (let ((commands (funcall data #'parse-command)))
    (time (reduce #'+ (map 'list #'volume (run-commands commands))))))
