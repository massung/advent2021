(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defun read-coords (board s &optional diagp)
  (re:with-re-match (m (re:match-re "(%d+),(%d+) %-> (%d+),(%d+)" s))
    (let* ((x1 (parse-integer $1))
           (y1 (parse-integer $2))
           (x2 (parse-integer $3))
           (y2 (parse-integer $4))

           ;; direction of travel
           (dx (signum (- x2 x1)))
           (dy (signum (- y2 y1))))
      (when (or (zerop dx)
                (zerop dy)
                (and diagp (= (abs (- x2 x1))
                              (abs (- y2 y1)))))
        (loop
           for i from 0 to (max (abs (- x2 x1))
                                (abs (- y2 y1)))
           do (let ((x (+ x1 (* dx i)))
                    (y (+ y1 (* dy i))))
                (incf (gethash (cons x y) board 0))))))))

(defun run (data diagp)
  (let ((board (make-hash-table :test #'equalp)))
    (funcall data #'(lambda (s) (read-coords board s diagp)))
    (loop for n being the hash-values in board when (> n 1) sum 1)))

(defun part-1 (&optional (data #'test-data))
  (run data nil))

(defun part-2 (&optional (data #'test-data))
  (run data t))
