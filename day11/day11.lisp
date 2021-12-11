(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defun parse-matrix (lines)
  (loop
     with m = (make-array '(10 10))
     for row from 0
     for line in lines
     do (loop
           for col from 0
           for c across line
           do (setf (aref m row col) (digit-char-p c)))
     finally (return m)))

(defun spark (m row col)
  (when (and (< -1 row 10)
             (< -1 col 10))
    (when (= (incf (aref m row col)) 10)
      (dolist (x '(-1 0 1))
        (dolist (y '(-1 0 1))
          (unless (and (zerop x)
                       (zerop y))
            (spark m (+ row y) (+ col x))))))))

(defun step-matrix (m)
  (dotimes (row 10)
    (dotimes (col 10)
      (spark m row col)))

  ;; count number of sparks that happened
  (let ((n 0))
    (dotimes (row 10 n)
      (dotimes (col 10)
        (when (> (aref m row col) 9)
          (setf (aref m row col) 0)
          (incf n))))))

(defun part-1 (&optional (data #'test-data))
  (let ((m (parse-matrix (funcall data))))
    (time (loop for i below 100 sum (step-matrix m)))))

(defun part-2 (&optional (data #'test-data))
  (let ((m (parse-matrix (funcall data))))
    (time (loop
             for i from 1
             for n = (step-matrix m)
             when (= n 100)
             return i))))
