(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defun parse-positions (s)
  (mapcar #'parse-integer (re:split-re "," s :all t)))

(defun summation (n)
  (multiple-value-bind (q r)
      (floor n 2)
    (+ (* (1+ n) q) (if (zerop r) 0 (1+ q)))))

(defun stddev (xs &aux (d 0) (m 0) (n 0))
  (dolist (x xs (values (sqrt (/ d (1- n))) m))
    (incf n)
    (let ((v (- x m)))
      (incf m (/ v n))
      (incf d (* v (- x m))))))

(defun fuel-used (xs n cost)
  (loop for x in xs sum (funcall cost (abs (- x n)))))

(defun run (xs &key (cost #'identity) (z 0.5))
  (multiple-value-bind (sd m)
      (stddev xs)
    (let ((a (floor (- m (* z sd))))
          (b (floor (+ m (* z sd)))))
      (loop for i from a to b minimizing (fuel-used xs i cost)))))

(defun part-1 (&optional (data #'test-data))
  (let ((positions (first (funcall data #'parse-positions))))
    (time (run positions))))

(defun part-2 (&optional (data #'test-data))
  (let ((positions (first (funcall data #'parse-positions))))
    (time (run positions :cost #'summation))))
