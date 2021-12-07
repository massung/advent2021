(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defun parse-ages (s)
  (mapcar #'parse-integer (re:split-re "," s :all t)))

(defun step-fish (v)
  (let ((spawned-fish (aref v 0)))
    (dotimes (i 8)
      (setf (aref v i) (aref v (1+ i))))
    (incf (aref v 6) spawned-fish)
    (setf (aref v 8) spawned-fish)))

(defun count-fish (ages days)
  (let ((v (map 'vector #'(lambda (i) (count i ages)) '(0 1 2 3 4 5 6 7 8))))
    (dotimes (i days (reduce #'+ v :initial-value 0))
      (step-fish v))))

(defun part-1 (&optional (data #'test-data))
  (let ((ages (first (funcall data #'parse-ages))))
    (time (count-fish ages 18))))

(defun part-2 (&optional (data #'test-data))
  (let ((ages (first (funcall data #'parse-ages))))
    (time (count-fish ages 256))))
