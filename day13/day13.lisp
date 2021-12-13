(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defun parse-coords (lines)
  (let ((coords (loop
                   for line = (pop lines)
                   while (plusp (length line))
                   collect (re:with-re-match (m (re:match-re "(%d+),(%d+)" line))
                             (list (parse-integer $1) (parse-integer $2))))))
    (values coords lines)))

(defun parse-folds (lines)
  (loop
     for line in lines
     collect (re:with-re-match (m (re:find-re "(%a)=(%d+)" line))
               (list (char $1 0) (parse-integer $2)))))

(defun parse-input (lines)
  (multiple-value-bind (coords rest)
      (parse-coords lines)
    (values coords (parse-folds rest))))

(defun build-matrix (coords)
  (loop
     with m = (make-hash-table :test 'equal)
     for coord in coords
     do (setf (gethash coord m) #\#)
     finally (return m)))

(defun fold-x (m pos)
  (loop
     with folded = (make-hash-table :test 'equal)
     for coord being the hash-keys of m
     for bit = (gethash coord m)
     for x = (first coord)
     for y = (second coord)
     do (cond
          ((< x pos)
           (setf (gethash coord folded) #\#))
          ((> x pos)
           (let ((opp (- pos (- x pos))))
             (setf (gethash (list opp y) folded) #\#))))
     finally (return folded)))

(defun fold-y (m pos)
  (loop
     with folded = (make-hash-table :test 'equal)
     for coord being the hash-keys of m
     for bit = (gethash coord m)
     for x = (first coord)
     for y = (second coord)
     do (cond
          ((< y pos)
           (setf (gethash coord folded) #\#))
          ((> y pos)
           (let ((opp (- pos (- y pos))))
             (setf (gethash (list x opp) folded) #\#))))
     finally (return folded)))

(defun fold-matrix (m folds)
  (flet ((fold (m fold)
           (destructuring-bind (axis pos)
               fold
             (case axis
               ((#\x) (fold-x m pos))
               ((#\y) (fold-y m pos))))))
    (reduce #'fold folds :initial-value m)))

(defun part-1 (&optional (data #'test-data))
  (multiple-value-bind (coords folds)
      (parse-input (funcall data))
    (time (let ((m (fold-matrix (build-matrix coords) (subseq folds 0 1))))
            (hash-table-count m)))))

(defun part-2 (&optional (data #'test-data))
  (multiple-value-bind (coords folds)
      (parse-input (funcall data))
    (time (let* ((m (fold-matrix (build-matrix coords) folds))
                 (coords (loop for coord being the hash-keys of m collect coord))
                 (w (1+ (reduce #'max coords :key #'first)))
                 (h (1+ (reduce #'max coords :key #'second))))
            (dotimes (y h)
              (dotimes (x w (terpri))
                (princ (gethash (list x y) m #\space))))))))
