(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defun part-1 (&optional (data #'test-data))
  (let ((lines (funcall data #'read-from-string)))
    (time nil)))

(defun part-2 (&optional (data #'test-data))
  (let ((lines (funcall data #'read-from-string)))
    (time nil)))
