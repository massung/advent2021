(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defun parse-positions (s)
  (mapcar #'parse-integer (re:split-re "," s :all t)))

(defun summation (n)
  (multiple-value-bind (q r)
      (floor n 2)
    (+ (* (1+ n) q) (if (zerop r) 0 (1+ q)))))

(defun steps (xs n cost)
  (reduce #'(lambda (a x) (+ a (funcall cost (abs (- x n))))) xs :initial-value 0))

(defun run (xs &optional (cost #'identity))
  (loop for i below (apply #'max xs) minimizing (steps xs i cost)))

(defun part-1 (&optional (data #'test-data))
  (let ((positions (first (funcall data #'parse-positions))))
    (run positions)))

(defun part-2 (&optional (data #'test-data))
  (let ((positions (first (funcall data #'parse-positions))))
    (time (run positions #'summation))))
