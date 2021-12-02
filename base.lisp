(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "../common/common.lisp"))

(defun part-1 (&optional (data #'test-data))
  (let ((lines (funcall data #'read-from-string)))
    nil))

(defun part-2 (&optional (data #'test-data))
  (let ((lines (funcall data #'read-from-string)))
    nil))
