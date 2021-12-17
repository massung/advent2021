(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defun parse-target-area (line)
  (with-re-match (m (find-re "x=(%-?%d+)..(%-?%d+), y=(%-?%d+)..(%-?%d+)" line))
    (list (list (parse-integer $1)
                (parse-integer $2))
          (list (parse-integer $3)
                (parse-integer $4)))))

(defun sim (proc v g initial-step min max)
  (do ((s initial-step (+ s 1))
       (x v (+ x v)))
      ((or (> x max) (and (zerop v) (minusp g) (< x min))))

    ;; only apply gravity if still moving or it's positive
    (when (or (plusp v) (plusp g))
      (incf v g))

    ;; if within the target range do callback
    (when (<= min x max)
      (unless (funcall proc s)
        (return-from sim)))))

(defun valid-time-steps (area)
  (let ((time-steps (make-hash-table)))
    (flet ((push-v (v)
             (lambda (s) (push v (gethash s time-steps)))))
      (loop
         with (min max) = (mapcar #'abs (reverse (second area)))
         for vy to max
         for down-steps = (sim (push-v (- vy)) vy 1 1 min max)
         for up-steps = (when (plusp vy)
                          (sim (push-v vy) (1+ vy) 1 (+ (* vy 2) 2) min max))
         finally (return time-steps)))))

(defun velocities (area)
  (loop
     with velocities = nil
     with time-steps = (valid-time-steps area)
     with max-time-step = (loop for s being the hash-keys of time-steps maximize s)
     with (min max) = (first area)
     for vx to max
     do (flet ((push-v (s)
                 (dolist (vy (gethash s time-steps) (< s max-time-step))
                   (push (list vx vy) velocities))))
          (sim #'push-v vx -1 1 min max))
     finally (return (remove-duplicates velocities :test 'equal))))

(defun summation (n)
  (multiple-value-bind (q r)
      (floor n 2)
    (+ (* (1+ n) q) (if (zerop r) 0 (1+ q)))))

(defun part-1 (&optional (data #'test-data))
  (let ((area (parse-target-area (first (funcall data)))))
    (time (summation (loop for (vx vy) in (velocities area) maximize vy)))))

(defun part-2 (&optional (data #'test-data))
  (let ((area (parse-target-area (first (funcall data)))))
    (time (length (velocities area)))))
