(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defun parse-line (s)
  (with-input-from-string (f s)
    (list (read f) (read f))))

(defun forward (n pos depth aim)
  (if aim
      (values (+ pos n) (+ depth (* aim n)) aim)
    (values (+ pos n) depth)))

(defun down (n pos depth aim)
  (if aim
      (values pos depth (+ aim n))
    (values pos (+ depth n))))

(defun up (n pos depth aim)
  (if aim
      (values pos depth (- aim n))
    (values pos (- depth n))))

(defun move (lines &optional aim &aux (pos 0) (depth 0))
  (dolist (cmd lines (* pos depth))
    (setf (values pos depth aim)
          (funcall (first cmd) (second cmd) pos depth aim))))

(defun part-1 (&optional (data #'test-data))
  (move (funcall data #'parse-line)))

(defun part-2 (&optional (data #'test-data))
  (move (funcall data #'parse-line) 0))
